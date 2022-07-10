select 10 + 7 * 3;

select (10 + 7) * 3;

select 10 - 7 * 3;

select (10 - 7) * 3;

select 10 + 7 / 3;

select (10 + 7) / 3;

select 10 + 7::int! * 3;

select 10 + !!7::int * 3;

select 10 + 7 * 3::int!;

select 10 + 7 * !!3::int;

select 10::int! + 7 * 3;

select !!10::int + 7 * 3;
