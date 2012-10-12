create database relalgebra;


use relalgebra;


create table persons (
  id      int    primary key,
  name    varchar(50),
  age     int,
  weight  int
);


create table pets (
  id        int     primary key,
  owner_id  int,
  name      varchar(50),
  species   varchar(50),
  age       int,
  foreign key (owner_id) references persons(id)
);


insert into persons values
          (1,  "Matt",  27,  160),
          (2,  "Kelly", 39,  110),
          (3,  "Abe",   15,  97),
          (4,  "Vera",  21,  135),
          (5,  "Rob",   52,  158),
          (6,  "Matt",  79,  143),
          (7,  "Vera",  31,  129);

insert into pets values
          (1,  1,   "King Kong", "dog",  17),
          (2,  3,   "Bozo",      "cat",  10),
          (3,  4,   "Hector",    "frog",  2),
          (4,  3,   "Ike",       "cat",   8),
          (5, null, "Achilles",  "cat",  15),
          (6, null, "Ike",       "frog",  1),
          (7,  4,   "Ike",       "cat",  12),
          (8,  4,   "Yoda",      "dog",  12),
          (9,  4,   "Clifford",  "frog",  2);
