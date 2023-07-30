# eBird API Inconsistencies and Peculiarities

The public eBird API is inconsistent and underspecified. This fact is in direct
opposition to our goal of modeling it in Haskell. In this document, we attempt
to enumerate the various pain points of the eBird API.

This has basically turned into a substitute for an eBird API issue tracker.

## Inconsistencies

Things in the API that don't line up but do "work".

* Observation detail levels

The "notable observations" endpoints allow a "detail" query parameter to
determine the detail level of the returned observations. Although one would
expect this query parameter to be handled by any endpoint that returns
observations, this is not the case.

We handle this by hard coding the returned observations of some endpoints to
"simple" detail level. Any endpoint that does properly handle detail levels
returns `SomeObservation` values, which are observations whose detail levels are
existentially quantified.

* Recent Checklists Feed

It is in the wrong spot of the eBird API documentation. Should be under the
`product` list, since that's the route.

* Clarify difference between checklist ID and submission ID

* Specifying mixed region types in observation endpoints

Errors with capture variable but is fine when mixed across capture variable and
"r" query param.

e.g.
```
curl "https://api.ebird.org/v2/data/obs/US-WY,US-WY-029/recent?maxResults=10&key=$(cat ~/.ebird/key.txt)"
```
errors

```
curl "https://api.ebird.org/v2/data/obs/US-WY/recent?r=US-WY-029&maxResults=15&key=$(cat ~/.ebird/key.txt)"
```
works

## Errors

Things that appear to be errors/bugs in the API.

* Observation endpoints do not handle "world" region

They return an error with "Region type custom not yet supported". But since the
"region info" endpoint does handle the "world" region, I would not expect this
to result in an error.

* Subregions of multiple regions errors

Specifying more than one region for the sub regions endpoint causes a 500 error,
where it seems like it should be prefectly acceptable to ask for the list of
counties in both US-WY and US-CO.

* Recent observations of species inconsistent with notable observations

I can see an observation of a species in the "recent notable observations"
output for a region, but I cannot see that same observation when searching for
observations of that species using the "recent species observations" endpoint.

* Historic observations endpoint does not respect region boundary

Requesting historic observations on this day (July 5th, 2023) in Park County,
Wyoming (US-WY-029) is showing observations from other counties.

* `includeProvisional` does not work

Try:
```
curl "https://api.ebird.org/v2/data/obs/US-WY-029/recent?includeProvisional=false&key=$(cat ~/.ebird/key.txt)"
```

Note that the response has observations with `"obsReviewed": false`.

* The regional statistics API does not work for subnational2 regions, always
  says 0 for all results

Try:
```
curl "https://api.ebird.org/v2/product/stats/US-WY-029/2023/7/15recent?&key=$(cat ~/.ebird/key.txt)"
```

Response has 0 for all fields.
