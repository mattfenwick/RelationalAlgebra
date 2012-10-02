create database relalgebra;


use relalgebra;


create table products (
  id int primary key,
  name varchar(50),
  store varchar(50),
  price decimal (10,2)
);


insert into products values
          (1,  "abc", "xyz", 32.23),
          (2,  "def", "www",  2.23),
          (3,  "abc", "www",  3.23),
          (4,  "def", "xyz",  32.3),
          (5,  "ghi", "xyz",  32.2),
          (6,  "abc", "www",   2.3),
          (7,  "abc", "www",   2.2),
          (8,  "ghi", "xyz",   3.2),
          (9,  "ghi", "www",   3.3),
          (10, "def", "xyz",     3),
          (11, "abc", "xyz",     2),
          (12, "jkl", "www",  19.7),
          (13, "mno", "xyz",  9.85),
          (14, "abc", "zzz", 10.22);
          
     
-- 1 row, 1 column: select an aggregate value on some column
create view group_1 as 
     select max(price)
     from products;
     
  
-- n rows, 1 column:  agg val, each group
create view group_2 as
    select
      store, 
      max(price) as max_price
    from products
    group by store;
     
    
-- augment each row with information from its group
create view group_aug as 
    select 
      p.*,
      l.max_price
    from
      products as p
    inner join
      group_2 as l
    on p.store = l.store;
    
    
-- select the row from each group that has the min/max/whatever-est value
--   Q: what if there's more than 1 in a group?
--   A: return multiple rows for that group
create view group_est as
    select 
      p.*
    from 
      products as p
    inner join
      group_2 as l
    on 
      p.store = l.store and 
      p.price = l.max_price;
      
      
create view pivot_1 as 
  select
    products.*,
    case when store = "www" then price end as WWW,
    case when store = "xyz" then price end as XYZ
  from products;
  
  
-- didn't feel like prettifying it
create view pivot_2 as
  select
    name,
    sum(WWW) as WWW,
    count(WWW) as WWW_ct,
    sum(XYZ) as XYZ,
    count(XYZ) as XYZ_ct
  from pivot_1
  group by name;
  
  
-- based on the procedure in my blog:
--   http://mfenwick100.blogspot.com/2012/04/relational-division.html
create table divisor (
  store varchar(50) primary key
);

insert into divisor values ("www"), ("xyz");
  

-- we'll use (name, store) of products as the dividend
create view dividend as
  select distinct
    name, store
  from products;
  
  
create view div_inner as
  select dividend.*
  from dividend
  inner join divisor
  on divisor.store = dividend.store;
  
  
create view division as
  select r.name
  from div_inner as r
  group by r.name
  having count(*) = (select count(*) from divisor);