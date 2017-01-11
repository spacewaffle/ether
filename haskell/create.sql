create table users (
  user_id serial primary key,
  username varchar,
  email varchar,
  encrypted_password varchar
)
