package com.allevite.jobsboard.config

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*
final case class StripeConfig(
    key: String,
    price: String,
    successUrl: String,
    cancelUrl: String
) derives  ConfigReader
