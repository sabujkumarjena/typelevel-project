CREATE TABLE users (
    email   text  NOT NULL,
    hashedPassword  text NOT NULL,
    firstName   text,
    lastName    text,
    company text,
    role    text NOT NULL
);

ALTER TABLE users
ADD CONSTRAINT pk_users PRIMARY KEY (email);

INSERT INTO users (
    email,
    hashedPassword,
    firstName,
    lastName,
    company,
    role
) values (
    'sabuj@allevite.com',
    '$2a$10$hCkLO1p6W4dAmFtgpCifquMszyQSkLAW94oYgOcv3uMK7.mY8rCfu',
    'Sabuj',
    'Jena',
    'Alle Vite',
    'ADMIN'
);

INSERT INTO users (
    email,
    hashedPassword,
    firstName,
    lastName,
    company,
    role
) values (
    'deepak@allevite.com',
    '$2a$10$cptmdsaJ2aTw4l2nzxOKDeUPysgwDqpd9Jir6BkcUiIKBc0VIcJfC',
    'Deepak',
    'Pradhan',
    'Alle Vite',
    'RECRUITER'
);