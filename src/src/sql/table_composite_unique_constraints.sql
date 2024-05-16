SELECT
  conkey
FROM
  pg_constraint
WHERE
  conrelid = $1
  AND contype = 'u'
  AND array_length(conkey, 1) > 1
