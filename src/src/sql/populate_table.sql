SELECT
  attnum,
  attname,
  pg_catalog.format_type(atttypid, atttypmod) AS atttype,
  attnotnull,
  (
    SELECT
      coalesce(array_agg(contype) FILTER (WHERE contype IS NOT NULL), '{}')
    FROM
      pg_constraint
    WHERE
      conrelid = attrelid
      AND conkey && ARRAY[attnum]
      AND (contype = 'p'
        OR contype = 'u'
        AND array_length(conkey, 1) = 1)) AS constraint
FROM
  pg_attribute
WHERE
  attrelid = $1
  AND attnum > 0
  AND NOT attisdropped
ORDER BY
  attnum
