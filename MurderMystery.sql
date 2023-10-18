-- This is a line comment (SQL murder mystery)
/*This is a block comment.
blabla
--*/

SELECT *
FROM crime_scene_report
where city="SQL City" and type="murder" and date=20180115;

/*Security footage shows that there were 2 witnesses.
The first witness lives at the last house on "Northwestern Dr".
The second witness, named Annabel, lives somewhere on "Franklin Ave".
--*/

SELECT*
FROM person
WHERE address_street_name ="Franklin Ave" and name like "Annabel%"
union
SELECT *
FROM person
WHERE address_street_name ="Northwestern Dr"
order by address_number desc
limit 1



SELECT *
FROM
(
SELECT *
FROM person
WHERE address_street_name ="Northwestern Dr"
order by address_number desc
limit 1
) as q1
union
SELECT *
FROM person
WHERE address_street_name ="Franklin Ave" and name like "Annabel%"

SELECT wt.name, it.transcript
FROM witnesses as wt--alias can be used for table
LEFT JOIN interview as it on it.person_id=wt.id

/*Morty Schapiro
I heard a gunshot and then saw a man run out.
He had a "Get Fit Now Gym" bag.
The membership number on the bag started with "48Z".
Only gold members have those bags.
The man got into a car with a plate that included "H42W".

Annabel Miller
I saw the murder happen, and I recognized the killer from my gym
when I was working out last week on January the 9th.
--*/

/*
Jeremy Bowers	I was hired by a woman with a lot of money.
I don't know her name but I know she's around 5'5" (65") or 5'7" (67").
She has red hair and she drives a Tesla Model S.
I know that she attended the SQL Symphony Concert 3 times in December 2017.
*/


--DROP TABLE suspect;
CREATE TABLE suspect
as
SELECT mem.name,interview.transcript
FROM get_fit_now_check_in as check_in
LEFT JOIN get_fit_now_member as mem on check_in.membership_id=mem.id
LEFT JOIN person on mem.person_id=person.id
LEFT JOIN drivers_license as dl on person.license_id=dl.id
LEFT JOIN interview on person.id=interview.person_id
where check_in_date='20180109'
and membership_status ='gold'
and membership_id like '48Z%'
and plate_number like '%H42W%';

SELECT *
FROM suspect



SELECT *
FROM drivers_license as dl
LEFT JOIN person on dl.id=person.license_id
LEFT JOIN income on income.ssn=person.ssn
LEFT JOIN
(SELECT person_id,count(*) as number
FROM facebook_event_checkin as FB
where event_name="SQL Symphony Concert"
	and date between 20171201 and 20171231
group by person_id
having count(*)=3)
as symphony --sub query
on symphony.person_id=person.id
where hair_color='red'
	and gender='female'
	and car_make='Tesla'
	and height between 65 and 67
	and number is not null
order by annual_income desc;