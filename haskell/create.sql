create table users (
  user_id serial primary key,
  username varchar not null unique,
  email varchar not null,
  encrypted_password varchar not null
)
