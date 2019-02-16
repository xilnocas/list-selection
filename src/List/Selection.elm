module List.Selection exposing
    ( Selection, fromList, toList
    , select, selectBy, deselect, selected
    , map, mapSelected, filter
    )

{-| This module exposes a list that has at most one selected item.

The invariants here:

  - You can select _at most_ one item.
  - You can't select an item that isn't part of the list.

But, these only hold if there are no duplicates in your list.


## Converting

@docs Selection, fromList, toList


## Selecting

@docs select, selectBy, deselect, selected


## Transforming

@docs map, mapSelected, filter

-}


{-| A list of items, one of which _might_ be selected.
-}
type Selection a
    = Selection Int (List a)


{-| Create a `Selection a` with nothing selected.
-}
fromList : List a -> Selection a
fromList items =
    Selection -1 items


{-| Convert a Selection list back to a regular list. This is useful
for creating view functions, for example. If you want a list that has
the selected item, use `mapSelected` like this:

    [ 1, 2, 3 ]
        |> fromList
        |> select 2
        |> mapSelected
            { selected = \a -> (True, a)
            , rest = \a -> (False, a)
            }
        |> toList
        --> [ (False, 1), (True, 2), (False, 3) ]

-}
toList : Selection a -> List a
toList (Selection _ items) =
    items


{-| Mark an item as selected. This will select at most one item. Any previously
selected item will be unselected.

    fromList ["Burrito", "Chicken Wrap", "Taco Salad"]
        |> select "Burrito"
        |> selected --> Just "Burrito"

Attempting to select an item that doesn't exist is a no-op.

    fromList ["Burrito", "Chicken Wrap", "Taco Salad"]
        |> select "Doner Kebab"
        |> selected --> Nothing

-}
select : a -> Selection a -> Selection a
select el selection =
    selectBy ((==) el) selection


{-| Mark an item as selected by specifying a function. This will select the
first item for which the function returns `True`. Any previously selected item
will be unselected.

    fromList ["Burrito", "Chicken Wrap", "Taco Salad"]
        |> selectBy (String.startsWith "B")
        |> selected --> Just "Burrito"

-}
selectBy : (a -> Bool) -> Selection a -> Selection a
selectBy query (Selection original items) =
    Selection
        (items
            |> List.indexedMap Tuple.pair
            |> List.filterMap
                (\( i, item ) ->
                    if query item then
                        Just i

                    else
                        Nothing
                )
            |> List.head
            |> Maybe.withDefault original
        )
        items


{-| Deselect any selected item. This is a no-op if nothing is selected in the
first place.
-}
deselect : Selection a -> Selection a
deselect (Selection _ items) =
    Selection -1 items


{-| Get the selected item, which might not exist.

    fromList ["Burrito", "Chicken Wrap", "Taco Salad"]
        |> select "Burrito"
        |> selected --> Just "Burrito"

-}
selected : Selection a -> Maybe a
selected (Selection selectedItem items) =
    items
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( i, item ) ->
                if i == selectedItem then
                    Just item

                else
                    Nothing
            )
        |> List.head


{-| Apply a function to all the items.

    fromList [1, 2, 3]
        |> map ((*) 2)
        |> toList --> [2, 4, 6]

-}
map : (a -> b) -> Selection a -> Selection b
map fn (Selection selectedItem items) =
    Selection
        selectedItem
        (List.map fn items)


{-| Apply a function to all the items, treating the selected item
specially.

    fromList [1, 2, 2]
        |> select 2
        |> mapSelected { selected = (*) 2, rest = identity }
        |> toList --> [1, 4, 2]

-}
mapSelected : { selected : a -> b, rest : a -> b } -> Selection a -> Selection b
mapSelected mappers (Selection selectedItem items) =
    Selection
        selectedItem
        (List.indexedMap
            (\i item ->
                if i == selectedItem then
                    mappers.selected item

                else
                    mappers.rest item
            )
            items
        )


{-| Filter all items where predicate evaluates to false.

    fromList [1, 2, 3]      -- (-1, [1,2,3])
        |> select 2         -- (1, [1,2,3])
        |> filter ((>) 2)   -- (-1, [1])
        |> toList --> [1]

    Preserves selected item when unfiltered.

    fromList [1,2,3]
        |> select 1
        |> filter (\x -> x <= 2)
        |> selected --> Just 1

-}
filter : (a -> Bool) -> Selection a -> Selection a
filter predicate ((Selection selectedIndex items) as selection) =
    let
        -- (new index, (old index, val))
        filteredSelection : List ( Int, ( Int, a ) )
        filteredSelection =
            items
                |> List.indexedMap Tuple.pair
                |> List.filter (Tuple.second >> predicate)
                |> List.indexedMap Tuple.pair
    in
    Selection
        (filteredSelection
            |> List.filterMap
                (\( new, ( old, _ ) ) ->
                    if old == selectedIndex then
                        Just new

                    else
                        Nothing
                )
            |> List.head
            |> Maybe.withDefault -1
        )
        (filteredSelection
            |> List.map (Tuple.second >> Tuple.second)
        )



-- case selectedItem of
--     Just selection ->
--         filteredSelection
--             |> select selection
--
--     Nothing ->
--         filteredSelection
