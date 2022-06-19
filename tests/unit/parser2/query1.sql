
select 1, 2;
SELECT id, name FROM characters;
SELECT id, name FROM characters WHERE id != 2;
SELECT id, name FROM characters WHERE name = 'Rachel';
SELECT id, name as charName FROM characters WHERE name != 'Rachel' AND id < 5;
SELECT name FROM characters ORDER BY name ASC;
SELECT DISTINCT (id / 2)::int FROM characters;
SELECT id::text || ' ' || name AS name_with_id FROM characters WHERE id > 1 ORDER BY id DESC LIMIT 4 OFFSET 5;
SELECT * FROM characters INNER JOIN character_roles ON characters.id=character_roles.character_id WHERE id != 2 ORDER BY id