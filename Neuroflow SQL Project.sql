with previous_query as (
select month(users.created_at) as start_month, year(users.created_at) as start_year, users.users as users, 
exercises.questionnaire_id, exercises.exercise_completion_date
from users
join exercises
on exercises.user_id = users.users
where datediff(day, users.created_at, exercises.exercise_completion_date) <= 30
)

select start_year, start_month, count(previous_query.users) as count,
count(previous_query.users)*1.0 /
(select count(0) as total_count from users 
where  month(users.created_at) = start_month and year(users.created_at) = start_year
) * 1.0 * 100 as percent_completed

from users
join previous_query
on previous_query.users = users.users
group by start_year, start_month;





