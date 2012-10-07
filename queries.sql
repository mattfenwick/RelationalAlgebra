use relalgebra;


select * from t;

select x, y from t;

select x, y, x + y from t;

select t.*, x + y from t;




select t.*, (select max(x) from t) from t;



select max(x) from t;



select count(*) from t;

select sum(x) from t;

select avg(x + y) from t;



select * from t where x < 3;

select * from t where y - x = 4;



select a, max(x) from t group by a;

select a, b, count(*), sum(x) from t group by a, b;



select x, y from t order by a desc limit 5;

select x + y from t order by b asc limit 5, 10; -- this starts with the 5th and ends with the 10th, right?  not sure

-- then we need a query that gets the top n from each group ... may be hard in SQL



-- how about a window-function like query?
-- where the 'window' is all the rows before the current one ... after some kind of sort

