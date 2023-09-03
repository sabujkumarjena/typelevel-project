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
    'allevite',
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
    'deepakrulez',
    'Deepak',
    'Pradhan',
    'Alle Vite',
    'RECRUITER'
);