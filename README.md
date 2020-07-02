# ECE Map

This repository contains the source code and data used by the _New Zealand Herald_ to create an interactive map of (almost) all Early Childhood Education centres in New Zealand. This map was used in the [_Choosing Childcare_](https://www.nzherald.co.nz/choosing-childcare/tags/1504924/) series - 
[for example (paywalled)](https://www.nzherald.co.nz/nz/news/article.cfm?c_id=1&objectid=12314559)

Here's a slightly low quality gif preview of map

![Map Preview](assets/ECE_map.gif)

The basis of the map is the [ECE Directory](https://www.educationcounts.govt.nz/data-services/directories/early-childhood-services) 
data provided by the Ministry of Education on the education counts site.

The ERO report information was scraped from the ERO website.

## Interactive Map

If you just want to reuse map you only need a working [node.js](https://nodejs.org/en/) setup and a mapbox
account.

Clone this repoistory and then in the `interactive` directory create the file `.env.dev` with:

```
MAPBOX_TOKEN=XXXX
```

where XXXX is your mapbox token.

Then from the `interactive` directory run `npm install` and then `npm run start`. This will open the map
in your browser.

The interative itself is a [react](https://reactjs.org/) app that uses [react-map-gl](http://visgl.github.io/react-map-gl/) as an interface to [mapbox](https://www.mapbox.com/)

## Data refresh

If you wish to recreate the ERO ratings data you will need working [haskell](https://www.haskell.org/) and [R](https://www.r-project.org/) environments.

- Download a fresh copy of the [ECE Directory] as CSV to `data/Directory-ECE-Current.csv`
- Open an R shell (this will automatically download and install renv) and run

```r
renv::restore()
```

- Install [haskell stack](https://docs.haskellstack.org/en/stable/README/) and then run

```sh
stack build --exec nzh
```

### The build system

Each ECE centre has a page on the ERO website that lists the last two ERO reports. The build firstly downloads each of these pages and then inspects those pages to find the links to the ERO reports. These are then downloaded and the ERO ratings are extracted from the reports.

The haskell build uses [shake](https://shakebuild.com/) to manage the process - the key file driving this is `Shakefile.hs`

## Copyright

The source code in this repository has been released until the MIT license by the _New Zealand Herald_.
The _New Zealand Herald_ has placed no restrictions on how you use our code - although if you do it would be great if you could let us know. If you have any questions please don't hesitate to contact Chris Knox either at chris.knox@nzherald.co.nz or https://twitter.com/vizowl 

All data in the repository was released under various creative commons licenses and retain their original ownership.

### Crown Copyright - Creative Commons Attribution 4

Originally from https://www.educationcounts.govt.nz/ for more detail see https://www.educationcounts.govt.nz/site-info/privacy

- `data/Directory-ECE-Current.csv`
- `data/Directory-ECE-Current.clean.csv`

### Copyright Education Review Office - Createive Commons Attribution-NonCommercial-NoDerivs 3.0 New Zealand (CC BY-NC-ND 3.0 NZ)

See https://www.ero.govt.nz/footer-lower/copyright/

All files in the directory `data/reports`

### Derived data products

The data files created using both the ECE directory and the ERO reports use the more restrictive of the two copyrights CC BY-NC-ND 3.0 NZ 
