{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ApiExample.Infrastructure.Aggregate.Person where

import ApiExample.Domain (Person (..))
import Data.Profunctor (Profunctor (dimap), lmap, rmap)
import Data.Text (Text)
import Data.Vector (Vector, fromList, snoc)
import Hasql.Statement (Statement (..))
import Hasql.TH (maybeStatement, resultlessStatement, vectorStatement)
import MyLib.Utils ()

multiInsert :: Statement ([Person]) ()
multiInsert =
  encode
    [resultlessStatement|
    insert into 
      person (person_id, age, full_name) 
    select * from unnest($1 :: text[], $2 :: int4[], $3 :: text[] )
    |]
 where
  encode = lmap $ foldl (\(a, b, c) Person{..} -> (snoc a personId, snoc b (fromIntegral age), snoc c fullName)) (mempty, mempty, mempty)

multiUpdate :: Statement [Person] ()
multiUpdate =
  encode
    [resultlessStatement|
    update person set 
      person_id = newdata.person_id,
      age = newdata.age,
      full_name = newdata.full_name
    from (select unnest($1 :: text[]) person_id, unnest($2 :: int4[]) age,unnest($3 :: text[]) full_name) newdata
    where
      person.person_id = newdata.person_id
    |]
 where
  encode = lmap $ foldl (\(a, b, c) Person{..} -> (snoc a personId, snoc b (fromIntegral age), snoc c fullName)) (mempty, mempty, mempty)

multiUpsert :: Statement [Person] ()
multiUpsert =
  encode
    [resultlessStatement|
    insert into 
      person (person_id, age, full_name) 
    select * from unnest($1 :: text[], $2 :: int4[], $3 :: text[] )
    on conflict (person_id) do update
    set 
      person_id = excluded.person_id,
      age = excluded.age,
      full_name = excluded.full_name
    |]
 where
  encode = lmap $ foldl (\(a, b, c) Person{..} -> (snoc a personId, snoc b (fromIntegral age), snoc c fullName)) (mempty, mempty, mempty)

findOne :: Statement Text (Maybe Person)
findOne =
  rmap
    (fmap \(personId, fullName, age) -> Person{age = fromIntegral age, ..})
    [maybeStatement|
    select 
      person_id :: text
      ,full_name :: text
      , age :: int4 
    from person 
    where person_id = $1 :: text
  |]

findMany :: Statement [Text] (Vector Person)
findMany =
  dimap
    fromList
    (fmap \(personId, fullName, age) -> Person{age = fromIntegral age, ..})
    [vectorStatement|
    select 
      person_id :: text
      ,full_name :: text
      , age :: int4 
    from person 
    where person_id = any($1 :: text[])
  |]

findAll :: Statement () (Vector Person)
findAll =
  (fmap \(personId, fullName, age) -> Person{age = fromIntegral age, ..})
    <$> [vectorStatement|
    select 
      person_id :: text
      ,full_name :: text
      , age :: int4 
    from person 

  |]

deleteById :: Statement [Text] ()
deleteById =
  lmap
    fromList
    [resultlessStatement|
    delete from person where person_id = any($1::text[])
  |]
