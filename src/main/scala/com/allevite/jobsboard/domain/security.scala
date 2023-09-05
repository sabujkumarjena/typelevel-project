package com.allevite.jobsboard.domain

import cats.*
import cats.implicits.*
import com.allevite.jobsboard.domain.user.*
import org.http4s.{Response, Status}
import tsec.authentication.{AugmentedJWT, JWTAuthenticator, SecuredRequest, TSecAuthService}
import tsec.authorization.{AuthorizationInfo, BasicRBAC}
import tsec.mac.jca.HMACSHA256

object security {
  type Crypto              = HMACSHA256
  type JwtToken            = AugmentedJWT[Crypto, String]
  type Authenticator[F[_]] = JWTAuthenticator[F, String, User, Crypto]
  type AuthRoute[F[_]]     = PartialFunction[SecuredRequest[F, User, JwtToken], F[Response[F]]]
  type AuthRBAC[F[_]]      = BasicRBAC[F, Role, User, JwtToken]

  // RBAC
  // BasicRBAC[F, Role, User, JwtToken]
  given authRole[F[_]: MonadThrow]: AuthorizationInfo[F, Role, User] with {
    override def fetchInfo(u: User): F[Role] = u.role.pure[F]
  }
  def allRoles[F[_]: MonadThrow]: AuthRBAC[F] =
    BasicRBAC.all[F, Role, User, JwtToken]

  def recruiterOnly[F[_]: MonadThrow]: AuthRBAC[F] =
    BasicRBAC(Role.RECRUITER)
  def adminOnly[F[_]: MonadThrow]: AuthRBAC[F] =
    BasicRBAC(Role.ADMIN)

// authorization
  case class Authorizations[F[_]](rbacRoutes: Map[AuthRBAC[F], List[AuthRoute[F]]])
  object Authorizations {
    given combiner[F[_]]: Semigroup[Authorizations[F]] = Semigroup.instance { (authA, authB) =>
      Authorizations(authA.rbacRoutes |+| authB.rbacRoutes)
    }
  }
// AuthRoute -> Authorizations -> TSecAuthService -> HttpRoute
// 1. AuthRoute -> Authorisation = .restrictedTo extension method

  extension [F[_]](authRoute: AuthRoute[F])
    def restrictedTo(rbac: AuthRBAC[F]): Authorizations[F] =
      Authorizations(Map(rbac -> List(authRoute)))
// 2. Authorization -> TSecAuthService = implicit conversion

  given auth2tsec[F[_]: Monad]: Conversion[Authorizations[F], TSecAuthService[User, JwtToken, F]] =
    authz => {
      // this responds with 401 always
      val unauthorizedService: TSecAuthService[User, JwtToken, F] =
        TSecAuthService[User, JwtToken, F] { _ =>
          Response[F](Status.Unauthorized).pure[F]
        }

      authz.rbacRoutes // map[RBAC, List[AuthRoute[F]]
        .toSeq
        .foldLeft(unauthorizedService) { case (acc, (rbac, routes)) =>
          // merge routes into one
          val bigRoute = routes.reduce(_.orElse(_))
          // build a new service, fall back to the acc if rbac/route fails
          TSecAuthService.withAuthorizationHandler(rbac)(bigRoute, acc.run)
        }
    }
// 3. semigroup for authorization
}
