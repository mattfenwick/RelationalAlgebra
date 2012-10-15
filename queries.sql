use relalgebra;


create view sel1 as 
  select * from persons;

create view sel2 as
  select age, weight from persons; -- hmm, duplicates?

create view sel3 as
  select age, weight, age + weight from persons;

create view sel4 as
  select persons.*, age + weight from persons;



create view agg1 as
  select 
    p1.*, 
    (select max(age) from persons) as max_age
  from persons p1;

create view agg2 as
  select max(weight) from persons;

create view agg3 as
  select count(*) from persons;

create view agg4 as 
  select sum(age) from persons;

create view agg5 as
  select avg(age + weight) from persons;



create view fil1 as 
  select * from persons where age < 30;

create view fil2 as
  select * from persons where weight in (110, 160);



create view grp1 as 
  select species, max(age) as max_age
  from pets group by species;

create view grp2 as 
  select 
    owner_id, species, count(*), sum(age) 
  from pets 
  group by owner_id, species;          
     
-- 1 row, 1 column: select an aggregate value on some column
create view grp3 as 
  select max(age)
  from persons;
     
-- augment each row with information from its group
create view grp4 as 
  select 
    pets.*, l.max_age
  from pets
  inner join grp1 as l
  on pets.species = l.species;
    
-- select the row from each group that has the min/max/whatever-est value
--   Q: what if there's more than 1 in a group?
--   A: return multiple rows for that group
create view grp5 as
  select pets.*
  from pets
  inner join grp1 as l
  on pets.species = l.species and 
     pets.age = l.max_age;



create view ord1 as 
  select name, age from persons order by weight desc limit 5;
  
-- this starts with the 5th and ends with the 10th, right?  not sure
create view ord2 as 
  select age + weight from persons order by name asc limit 2, 4;

-- then we need a query that gets the top n from each group ... may be hard in SQL



-- how about a window-function like query?
-- where the 'window' is all the rows before the current one ... after some kind of sort


-- --------------------------
-- Subqueries

-- scalar subquery
create view sub1 as
  select 
    pets.*, (select count(*) from pets) as num_pets
  from pets;
  
create view sub2 as 
  select * from pets
  where species = (select max(species) from pets);
  
create view sub3 as
  select * from pets p1
  where 2 = (select count(*) from pets p2 where p2.age = p1.age);
  
-- this should be identical to sub3
create view sub3_ as
  select * 
  from pets p1 
  inner join sub4_sub p2
    on p1.age = p2.a1 
  where p2.num = 2; 
  
create view sub4_sub as 
  select age as a1, count(*) as num 
  from pets 
  group by a1;
  
-- any, in, some, all
create view sub4 as 
  select * from pets 
  where age > any (select age from pets); -- 'some' is the same as 'any'
  
create view sub5 as
  select * from pets
  where age in (select age from persons);
  
-- should be identical to sub5
create view sub5_ as
  select * from pets
  where exists (select * from persons where pets.age = persons.age);
  
-- should also be identical to sub5
create view sub5__ as
  select pets.* from pets
  inner join persons
  on pets.age = persons.age;
  
create view sub6 as 
  select * from pets
  where age not in (1,2,3,4,5); -- 'not in' equals '<> all'
  
-- these '<> some' queries are weird
--   I guess this just means, there's at least 1 that it's not equal to ...
create view sub7 as
  select * from pets
  where age <> some (select age from persons);
  
create view sub8 as
  select * from pets
  where age <> some (select age from persons where age = 15);
  
create view sub9 as
  select * from persons
  where age > all (select age from pets);
  
-- this is a dumb query, b/c if 'persons' is null,
-- it blows up, and MySQL rejects it you have 'limit 2' etc.
create view sub10 as
  select * from persons
  where 30 > (select age from persons limit 1);
  
-- weird ones, because the subquery could return null,
-- which would blow up this query
create view sub11 as
  select * from persons
  where age * 2 >= (select max(age) from persons);
  
-- row subqueries:  already covered?
create view sub12 as
  select * from persons
  WHERE (id, age) in (select id, age from persons where id <= 3);
  
create view sub13 as
  select * from pets
  where (owner_id, age) = (4, 12);
  
create view sub14 as
  select * from persons
  where exists (select * from pets where persons.id = pets.owner_id);
  
create view sub15 as
  select * from persons
  where not exists (select * from pets where persons.id = pets.owner_id);
  
-- same as sub15
create view sub15_ as
  select persons.* from persons
  left join pets
  on persons.id = pets.owner_id
  where pets.owner_id is null;

-- subqueries in from clause ... oh yeah, doesn't work for views!!!
create view sub16_help as
  select name, count(*) as num from persons group by name;
  
create view sub16 as
  select avg(num) from sub16_help;
  
  
  
  
-- some more complicated queries
      
      
create view pivot_1 as 
  select
    pets.*,
    case when species = "dog" then species end as dog,
    case when species = "cat" then species end as cat,
    case when species = "frog" then species end as frog
  from pets;
  
  
-- didn't feel like prettifying it
--   ***this doesn't make any sense!!!
-- can replicate with:
--  select
--    owner_id,
--    count(*) as num,
--    sum(age) as sum_age,
--    avg(age) as avg_age
--  from pets
--  group by owner_id;
-- doesn't even need pivot_1 table !!!
create view pivot_2 as
  select
    owner_id,
    count(*) as num,
    sum(age) as sum_age,
    avg(age) as avg_age
  from pivot_1
  group by owner_id;
  
  
-- based on the procedure in my blog:
--   http://mfenwick100.blogspot.com/2012/04/relational-division.html
create table divisor (
  species varchar(50) primary key
);

insert into divisor values ("cat"), ("frog");
  

-- we'll use (owner_id, species) of pets as the dividend
create view dividend as
  select distinct
    owner_id, species
  from pets;
  
  
create view div_inner as
  select dividend.*
  from dividend
  inner join divisor
  on divisor.species = dividend.species;
  
  
create view quotient as
  select r.owner_id
  from div_inner as r
  group by r.owner_id
  having count(*) = (select count(*) from divisor);

