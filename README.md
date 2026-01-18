# American political data & R

*Updated: 2026-01-18*

![](README_files/figure-markdown_github/collage1.png)

<br>

**An R-based guide** to accessing, exploring & visualizing US political
data via a collection of publicly available resources.

Election returns have been collated from [Daily
Kos](https://www.dailykos.com/), [MIT Election Data and Science
Lab](MIT%20Election%20Data%20and%20Science%20Lab) and Wikipedia; the R
package [Rvoteview](https://github.com/voteview/Rvoteview) is used
extensively to characterize lawmakers and congress.

**January 2026 version**. Most plots have been updated, some added, some
abandoned. I have removed all census based examples this time around.
And focused more on national-level Presidential election results.
Previous versions are available
[here](https://github.com/jaytimm/american-political-data-and-r/tree/master/versions).

Hopefully **a useful open source & transparent framework** for
investigating past & future election results and congresses using R. All
work presented here can be reproduced in its entirety.

-   [American political data & R](#american-political-data-r)
-   [Quick preliminaries](#quick-preliminaries)
    -   [Some geo-spatial data](#some-geo-spatial-data)
        -   [State-based geo-data](#state-based-geo-data)
    -   [A simple add-on map theme](#a-simple-add-on-map-theme)
    -   [Some quick definitions](#some-quick-definitions)
-   [Data sources](#data-sources)
    -   [VoteView](#voteview)
    -   [PresElectionResults](#preselectionresults)
    -   [Legislator details](#legislator-details)
-   [Historical presidential election
    results](#historical-presidential-election-results)
    -   [National popular vote is becoming more
        competitive](#national-popular-vote-is-becoming-more-competitive)
    -   [National popular vote and electoral landslides in the 20th
        century](#national-popular-vote-and-electoral-landslides-in-the-20th-century)
    -   [Voting margins in Presidential elections by state
        1980-2024](#voting-margins-in-presidential-elections-by-state-1980-2024)
    -   [When each state last voted for a Democratic presidential
        nominee](#when-each-state-last-voted-for-a-democratic-presidential-nominee)
    -   [Presidential elections and vote shares by
        state](#presidential-elections-and-vote-shares-by-state)
    -   [Presidential elections and the disappearance of competitive
        counties](#presidential-elections-and-the-disappearance-of-competitive-counties)
-   [Historical composition of the
    Senate](#historical-composition-of-the-senate)
    -   [Split Senate delegations and shifting
        ideologies](#split-senate-delegations-and-shifting-ideologies)
    -   [Split Senate delegations on the wane
        again](#split-senate-delegations-on-the-wane-again)
    -   [Split Senate delegations in the current
        Congress](#split-senate-delegations-in-the-current-congress)
    -   [US Senate delegations by party
        composition](#us-senate-delegations-by-party-composition)
    -   [Republican Senators and a minority of
        Americans](#republican-senators-and-a-minority-of-americans)
-   [Historical composition of the
    House](#historical-composition-of-the-house)
    -   [Political realignment in the
        South](#political-realignment-in-the-south)
    -   [On the evolution of the Southern
        Republican](#on-the-evolution-of-the-southern-republican)
-   [Age, generations & freshman classes in the
    House](#age-generations-freshman-classes-in-the-house)
    -   [Average age of House members](#average-age-of-house-members)
    -   [Introducing Generation Z](#introducing-generation-z)
    -   [First-timers in the House](#first-timers-in-the-house)
-   [Towards 2026](#towards-2026)
    -   [Class II Senators](#class-ii-senators)
    -   [Vulnerable Republican House
        Members](#vulnerable-republican-house-members)
-   [Fin](#fin)

## Quick preliminaries

``` r
library(dplyr)
library(ggplot2)
```

### Some geo-spatial data

#### State-based geo-data

``` r
library(sf)
library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")

nonx <- c('78', '69', '66', '72', '60', '15', '02')

states_sf <- tigris::states(cb = TRUE) |>
  rename(state_code = STATEFP, state_abbrev = STUSPS)

states <- states_sf |>
  data.frame() |>
  select(state_code, state_abbrev)

laea <- sf::st_crs("+proj=laea +lat_0=30 +lon_0=-95") 
```

### A simple add-on map theme

``` r
theme_guide <- function () {
  
    theme(axis.title.x=element_blank(), 
          axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          legend.title=element_blank(),
          legend.position = 'none', 
          complete = F) }
```

### Some quick definitions

> Per VoteView definition: The South = Dixie + Kentucky + Oklahoma

``` r
south <- c('SC', 'MS', 'FL', 
           'AL', 'GA', 'LA', 'TX', 
           'VA', 'AR', 'NC', 'TN',
           'OK', 'KY')
```

``` r
states_sf |>
  filter(!state_code %in% nonx) |>
  mutate(south = ifelse(state_abbrev %in% south, 
                        'south', 'not')) |>
  select(state_abbrev, geometry, south) |>
  mutate(label = ifelse(!grepl('not', south), state_abbrev, NA)) |>
  sf::st_transform(laea) |>
  
  ggplot() + 
  geom_sf(aes(fill = south),
          color = 'white', size = .15,
          alpha = 0.65) +
  geom_sf_text(aes(label = label),
                          size = 2.25,
                          color='black') +
  
  scale_fill_manual(values = c('#8faabe', 
                               '#1a476f', 
                               '#55752f')) +  
  theme_minimal() + 
  theme_guide() +
  theme(panel.background = 
          element_rect(fill = '#d5e4eb', color = NA)) +
  ggtitle('Dixie + Kentucky + Oklahoma')
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

------------------------------------------------------------------------

## Data sources

### VoteView

> The [VoteView](https://voteview.com/) project provides roll call-based
> political ideology scores for all lawmakers in the history of the US
> Congress. The R package `Rvoteview` provides access to these data.

``` r
## NOTE: election years.  term begins year + 1
ccr <- data.frame(year = c(1786 + 2*rep(c(1:119))), 
                  congress = c(1:119)) 
```

``` r
con <- 66 #66

vvo <- lapply(c('house', 'senate'), function(x) {
              Rvoteview::download_metadata(type = 'members', 
                                    chamber = x) |>
    filter(chamber != 'President') }) 
```

    ## [1] "/tmp/RtmpMzgyG7/Hall_members.csv"
    ## [1] "/tmp/RtmpMzgyG7/Sall_members.csv"

``` r
congress00 <- vvo |>
  bind_rows() |>
  mutate(x = length(unique(district_code))) |>
    ungroup() |>
    mutate(district_code = ifelse(x==1, 0, district_code)) |>
    mutate(district_code = 
             stringr::str_pad (as.numeric(district_code), 
                               2, pad = 0),
           
           southerner = ifelse(state_abbrev %in% south, 
                        'South', 'Non-south'),
           party_name = case_when (party_code == 100 ~ 'Democrat',
                                   party_code == 200 ~ 'Republican',
                                   !party_code %in% c(100, 200) ~ 'other')) |>
  
  left_join(ccr, by = 'congress') |>
  filter(!is.na(born))

congress <- congress00 |>
  filter(congress > con)
```

### PresElectionResults

> The [`PresElectionResults`
> package](https://github.com/jaytimm/PresElectionResults) includes US
> Presidential Election Results by county (2000-2024), congressional
> district (2024), and state (1864-2024). Additionally included are FRED
> population data and equal-area simple feature geometries (via Daily
> Kos). Full package build details are available
> [here](https://github.com/jaytimm/PresElectionResults/blob/master/builds.md).

``` r
devtools::install_github("jaytimm/PresElectionResults")
```

### Legislator details

> Via
> [unitedstates.github.io](https://github.com/unitedstates/congress-legislators)

``` r
leg_dets <- 'https://unitedstates.github.io/congress-legislators/legislators-current.csv'
leg_dets0 <- read.csv((url(leg_dets)), stringsAsFactors = FALSE) |>
  rename(state_abbrev = state)
```

------------------------------------------------------------------------

## Historical presidential election results

> National popular and electoral presidential election results made
> available at
> [britannica.com](https://www.britannica.com/topic/United-States-Presidential-Election-Results-1788863).

### National popular vote is becoming more competitive

``` r
library(ggplot2)
pres1 <- PresElectionResults::pres_results |>
  filter(year > 1864, 
         party %in% c('Democratic', 'Republican'),
         !is.na(pop_per)) |>
  select(year, party, pop_per) |>
  tidyr::spread(party, pop_per) |>
  
  mutate(delta = Republican - Democratic,
         d20 = ifelse(year %% 20 == 4, year, '')) 

pres1 |> 
  ggplot2::ggplot(aes(x = delta,
                      y = year |> as.character(),
                      group=1,
                      color = delta)) + 

  geom_vline(xintercept = 0, color = 'gray') +
  geom_path(color = 'gray', size = 0.5) + 
  geom_point(size = 4) +
  scale_color_gradient2(low = "#5f8bd7", 
                        mid = "#8366a5", 
                        high = "#e75848",
                        midpoint = 0) +
  
  scale_y_discrete(limits = rev, labels = pres1$d20 |> rev()) + 
  theme_minimal() + 
  theme(axis.ticks = element_blank(),
        legend.position = 'none') +
  
  xlim(-30, 30) +
  labs(title ='National popular vote margins',
       subtitle = '1868 to 2024',
       caption = 'Data source: Britannica')
```

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)

### National popular vote and electoral landslides in the 20th century

``` r
PresElectionResults::pres_results |>
  filter(year > 1828, !is.na(ec_votes)) |>
  group_by(year) |>
  filter(ec_votes == max(ec_votes)) |> ungroup() |>
  mutate(ec_per = round(ec_votes/ec_total*100,1)) |>
  tidyr::pivot_longer(cols = c(pop_per, ec_per)) |>
  
  ggplot(aes(x = value, 
             y = year)) +
  
  geom_line(aes(group = year), 
            color = 'lightgray', 
            size = 2) +
  geom_point(aes(color = name)) +
  xlim(0,100) +
  coord_flip() +
  theme_minimal() + 
  ggthemes::scale_colour_economist() +
  theme(axis.ticks = element_blank(),
      legend.position = 'none') +
  scale_y_continuous(breaks = seq(1824, 2024, by = 20)) +
  labs(title ="President-elect's share of electoral and popular votes",
       subtitle = '1828 to 2024',
       caption = 'Data source: Britannica')
```

![](README_files/figure-markdown_github/unnamed-chunk-12-1.png)

### Voting margins in Presidential elections by state 1980-2024

> Historical Presidential election results by state [via
> Wikipedia](https://github.com/jaytimm/uspols#4-wikipedia-presidential-returns-by-state-1864-).
> Equal-area state geometry via [Daily
> Kos](https://docs.google.com/spreadsheets/d/1LrBXlqrtSZwyYOkpEEXFwQggvtR0bHHTxs9kq4kjOjw/edit#gid=1278876419).

``` r
mp <- PresElectionResults::xsf_TileOutv10 |>
  left_join(PresElectionResults::pres_by_state |>
              filter(year >= 1980, year <= 2024) |>
              mutate(margins = republican - democrat)) 

mp |> 
  ggplot() +  
  geom_sf(aes(fill = margins),
           color = 'darkgray', lwd = .15) +
  geom_sf(data = PresElectionResults::xsf_TileInv10, 
          fill = NA, 
          show.legend = F, 
          color = NA, 
          lwd=.5) +
  
  geom_sf_text(data = PresElectionResults::xsf_TileInv10,
                          aes(label = state_abbrev),
                          size = 1.5,
                          color='black') +
  
  scale_fill_distiller(palette = "RdBu",  
                        limit = max(abs(mp$margins)) * c(-1, 1)) +
  facet_wrap(~year, ncol = 4) +
  theme_minimal()+ theme_guide() +
  labs(title = "Voting margins in Presidential elections 1980-2024",
       caption = 'Data sources: Wikipedia & Daily Kos')
```

![](README_files/figure-markdown_github/unnamed-chunk-13-1.png)

### When each state last voted for a Democratic presidential nominee

``` r
clean_prex <-  PresElectionResults::pres_by_state |>
  mutate(winner = gsub('Franklin D. Roosevelt', 'FDR', winner),
         winner = gsub('Lyndon B. Johnson', 'LBJ', winner),
         winner = gsub('Hillary Clinton', 'HRC', winner)) 
```

``` r
last_dem <- clean_prex |>
  group_by(state_abbrev, party_win) |>
  filter(year == max(year),
         party_win == 'democrat') |>
  ungroup() |>
  mutate(lab = paste0(year, ' - ', winner))
```

> **Nine US states** have not voted for a Democratic Presidential
> candidate since LBJ.

``` r
new1 <- PresElectionResults::xsf_TileInv10 |> 
  left_join(last_dem, by ='state_abbrev') |>
  mutate(label = paste0(state_abbrev, 
                        '\n', 
                        year,
                        '\n', 
                        gsub('^.* ', '', winner)))

PresElectionResults::xsf_TileOutv10 |> 
  left_join(last_dem, by ='state_abbrev') |>
  arrange(desc(year)) |>
  ggplot() + 
  geom_sf(aes(fill = paste0(year, ' - ', gsub('^.* ', '', winner))),
          color = 'gray' , 
          alpha = .5) + 
  
  geom_sf_text(data = new1,
                          aes(label = new1$label), 
                          size = 3,
                          color = 'black') +
  theme_minimal() + 
  theme_guide() + 
  ggthemes::scale_fill_economist()+
  labs(title = "When each state last voted for a Democratic presidential nominee",
       caption = 'Data sources: Wikipedia & Daily Kos')
```

![](README_files/figure-markdown_github/unnamed-chunk-16-1.png)

### Presidential elections and vote shares by state

``` r
vote_share <- clean_prex |>
  select(-party_win) |>
  tidyr::gather(key = 'party', value = 'per', democrat:republican) |>
  group_by(state_abbrev) |>
  slice(which.max(per)) |>
  ungroup() |>
  mutate(label = paste0(year, ' - ', winner))
```

``` r
new <- PresElectionResults::xsf_TileInv10 |> 
  left_join(vote_share, by ='state_abbrev') |>
  mutate(per = round(per, 1)) |>
  mutate(label = paste0(state_abbrev, 
                        '\n', 
                        year,
                        '\n', 
                        gsub('^.* ', '', winner), 
                        '\n',
                        per))

PresElectionResults::xsf_TileOutv10 |> 
  left_join(vote_share, by ='state_abbrev') |>
  ggplot() + 
  geom_sf(aes(fill = paste0(year, ' - ', gsub('^.* ', '', winner))),
          color = 'white' , 
          alpha = .65) + 
  
  geom_sf_text(data = new,
                          aes(label = new$label), 
                          size = 2.5,
                          color = 'black') +
  scale_fill_manual(
      values = colorRampPalette(ggthemes::economist_pal()(8))(18)) +
  
  theme_minimal() + theme_guide() + 
  labs(title = "Largest vote share for Presidential nominee",
  subtitle = "By state since 1864",
  caption = 'Data sources: Wikipedia & Daily Kos')
```

![](README_files/figure-markdown_github/unnamed-chunk-18-1.png)

### Presidential elections and the disappearance of competitive counties

``` r
counties <- tigris::counties(cb = TRUE) |> 
  filter(!STATEFP %in% nonx) |>
  sf::st_transform(laea)
```

``` r
cutoff <- 10

cl2 <- PresElectionResults::pres_by_county |>
  mutate(delta = republican - democrat,
         dcat = case_when (delta < -(cutoff -1) ~ 'DEM > +10',
                           delta > (cutoff -1) ~ 'REP > +10',
                           delta > -cutoff & delta < cutoff ~ 'competitive'))

cl2 |> 
  count(year, dcat) |>
  filter(!is.na(dcat)) |>
  tidyr::spread(dcat, n) |>
  knitr::kable()
```

| year | competitive | DEM \> +10 | REP \> +10 |
|-----:|------------:|-----------:|-----------:|
| 2000 |         734 |        382 |       2036 |
| 2004 |         586 |        332 |       2236 |
| 2008 |         663 |        577 |       1914 |
| 2012 |         516 |        503 |       2138 |
| 2016 |         299 |        370 |       2488 |
| 2020 |         302 |        410 |       2443 |
| 2024 |         312 |        361 |       2514 |

------------------------------------------------------------------------

## Historical composition of the Senate

### Split Senate delegations and shifting ideologies

> A **Senate delegation** for a given state is said to be **split** when
> comprised of Senators from different parties, eg, one Republican and
> one Democrat – as is the case with, eg, Pennsylvania in the (present)
> 119th Congress.

``` r
sens <- congress00 |>
  filter(chamber == 'Senate') |>
  mutate(party_name = as.factor(party_name)) |>
  mutate(party_name = forcats::fct_relevel(party_name, 
                                       'other', 
                                        after = 2)) |>
  mutate(year = year + 1) |>
  group_by(year, congress, state_abbrev) |>
  slice(1:2) |>
  ungroup() 
```

``` r
sens2 <- sens |>
  filter(congress %in% c(71, 77, 83,
                         89, 95, 101, 
                         107, 113, 119)) |>
  
  group_by(year, congress, state_abbrev) |>
  arrange (party_name) |>
  mutate(layer = row_number()) |>
  ungroup()
```

``` r
PresElectionResults::xsf_TileOutv10 |>
  left_join(sens2 |> filter(layer == 2)) |>
  ggplot() + 
  geom_sf(aes(fill = party_name),
          color = 'white', 
          lwd = 0.2,
          alpha = .85) + 
  
  geom_sf(data = PresElectionResults::xsf_TileInv10 |>
            left_join(sens2 |> filter (layer == 1)), 
          aes(fill = party_name),
          color = 'white', 
          lwd = 0.2,
          alpha = .7) +
  
  geom_sf_text(data = PresElectionResults::xsf_TileInv10,
                          aes(label = state_abbrev), 
                          size = 1.55,
                          color = 'white') +
  
  ggthemes::scale_fill_stata()+
  theme_minimal() + 
  theme_guide() +
  theme(legend.position = 'bottom') +
  
  facet_wrap(~year + congress) +
  labs(title = "Senate composition by state since 1929",
       caption = 'Data sources: Daily Kos & VoteView')
```

![](README_files/figure-markdown_github/unnamed-chunk-23-1.png)

### Split Senate delegations on the wane again

``` r
split_senate <- sens |>
  filter(congress > con) |>
  group_by(year, congress, state_abbrev) |>
  summarize(splits = length(unique(party_name)),
            parts = paste0(party_name, collapse = '-')) |>
  mutate(parts = ifelse(splits == 2, 
                        'Split', 
                        paste0('Both ', 
                               gsub('-.*$', '', parts)))) 

split_senate$parts <- factor(split_senate$parts, 
                             levels = c('Both Democrat', 
                                        'Split',
                                        'Both Republican', 
                                        'Both other')) 
```

``` r
split_senate |>
  filter(splits == 2) |>
  group_by(year, congress) |>
  summarize(n = n()) |>
  ggplot() +
  geom_bar(aes(x = year, 
               y = n), 
           color = 'white',
           fill = 'steelblue',
           stat = 'identity') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x=element_blank()) +
  scale_x_continuous(breaks = seq(1921, 2025, 4)) +
  labs(title = "Split Senate delegations since 1921",
       caption = 'Data source: VoteView')
```

![](README_files/figure-markdown_github/unnamed-chunk-25-1.png)

### Split Senate delegations in the current Congress

> Note: King (Maine) and Sanders (Vermont) are Independents that caucus
> with Democrats, so there are only two actual splits with different
> party control.

``` r
split_senate_details <- sens |>
  filter(congress > con) |>
  filter(congress == max(congress)) |>
  group_by(year, congress, state_abbrev) |>
  filter(n_distinct(party_name) == 2) |>
  arrange(party_name) |>
  mutate(senator_num = row_number()) |>
  ungroup() |>
  select(congress, year, state_abbrev, bioname, party_name, senator_num) |>
  tidyr::pivot_wider(names_from = senator_num, 
                     values_from = c(bioname, party_name),
                     names_sep = "_") |>
  select(congress, year, state_abbrev, 
         senator_1 = bioname_1, party_1 = party_name_1,
         senator_2 = bioname_2, party_2 = party_name_2) |>
  arrange(state_abbrev)

split_senate_details |> knitr::kable()
```

| congress | year | state_abbrev | senator_1 | party_1 | senator_2 | party_2 |
|------:|----:|:---------|:----------------|:--------|:-----------------|:--------|
| 119 | 2025 | ME | COLLINS, Susan Margaret | Republican | KING, Angus Stanley, Jr. | other |
| 119 | 2025 | PA | FETTERMAN, John Karl | Democrat | MCCORMICK, David Harold | Republican |
| 119 | 2025 | VT | WELCH, Peter | Democrat | SANDERS, Bernard | other |
| 119 | 2025 | WI | BALDWIN, Tammy | Democrat | JOHNSON, Ron | Republican |

### US Senate delegations by party composition

``` r
split_pal <- c('#395f81', '#ead8c3', '#9e5055', '#b0bcc1')

split_senate |>
  group_by(year, congress, parts) |>
  summarize(n = n()) |>

  ggplot(aes(x = year, 
             y = n, 
             fill = parts))+
  geom_bar(alpha = 0.85, 
           color = 'gray', 
           lwd = .25,
           stat = 'identity') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position = "bottom",
        legend.title=element_blank())+
  scale_fill_manual(values = split_pal) +
  scale_x_continuous(breaks = seq(1921, 2025, 4)) +
  xlab('') +
  labs(title = 'US Senate delegations, by party composition',
       caption = 'Data source: VoteView')
```

![](README_files/figure-markdown_github/unnamed-chunk-27-1.png)

### Republican Senators and a minority of Americans

> Senate seats held by Republicans via VoteView; population of states
> represented by Reublican senators via FRED.

``` r
wpops <- sens |> #yy |> 
  filter(congress > con) |>
  left_join(PresElectionResults::fred_pop_by_state,
            by = c('year', 'state_abbrev')) |> #yy |>
  group_by(year, party_name) |>
  summarize(n = n(),
            #n = sum(n),
            pop = sum(population)) |>
  group_by(year) |>
  mutate(Senate_share = round(n/sum(n) * 100, 1),
         Population_share = round(pop/sum(pop) * 100, 1)) |>
  filter(party_name == 'Republican') |>
  select(year, Senate_share, Population_share) |>
  tidyr::gather(-year, key = 'var', value = 'per')
```

> *Gray highlight*: Congresses in which (1) GOP senators hold a majority
> in the Senate AND (2) Republican senators collectively represent less
> than half of the U.S. population. We are presently in a
> Republican-controlled Senate where Republican senators represent a
> minority of Americans.

``` r
wpops |>
  ggplot() +
  geom_rect(aes(xmin = 2015, 
                xmax = 2019,
                ymin = -Inf, 
                ymax = Inf),
            fill = 'lightgray') +
  
   geom_rect(aes(xmin = 2024, 
                xmax = 2026,
                ymin = -Inf, 
                ymax = Inf),
            fill = 'lightgray') +
  
  geom_hline(yintercept = 50, color = 'black', lwd = .2) +
  geom_line(aes(x = year, 
                y = per, 
                color = var), 
            size = 1) +
  ggthemes::scale_color_few()+
  theme_minimal() +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  
  scale_x_continuous(breaks = seq(min(wpops$year), max(wpops$year), 4)) +
  labs(subtitle = "Republican Senate share v. Share Americans represented by Republican senator",
       caption = 'Data sources: VoteView & FRED') 
```

![](README_files/figure-markdown_github/unnamed-chunk-29-1.png)

------------------------------------------------------------------------

## Historical composition of the House

``` r
congress_south <- congress |> 
  filter(party_code %in% c(100, 200), chamber == 'House') |>
  mutate(Member = as.factor(paste0(party_name, ', ', southerner))) |>
  mutate(Member = forcats::fct_relevel(Member, 
                                       'Republican, Non-south', 
                                       after = 3)) 
```

### Political realignment in the South

``` r
congress_south |>
  group_by(year, Member) |>
  summarize(n = n()) |>
  mutate(n = n/sum(n)) |>
  
  ggplot(aes(x = year+1, 
             y = n, 
             fill = Member)) +
  geom_area(alpha = 0.65, color = 'gray') +
  
  geom_hline(yintercept = 0.5, color = 'white', linetype = 2) +

  scale_x_continuous(breaks=seq(min(congress_south$year+1),
                                max(congress_south$year+ 1), 4)) +
  scale_fill_manual(values = c('#1a476f', '#8faabe',
                                '#e19463', '#913a40')) +
  
  theme_minimal() + 
  theme(legend.position = 'top',
        legend.title=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  labs(title = "House composition since 1920",
       caption = 'Data source: VoteView')
```

![](README_files/figure-markdown_github/unnamed-chunk-31-1.png)

### On the evolution of the Southern Republican

> **DW-NOMINATE ideal points in two dimensions**. The first dimension
> captures ideological variation based in the standard
> liberal-conservative divide. The second captures variation based in
> social conservatism that crosscuts political affiliation.

``` r
congress_south |>
  mutate(year = year + 1) |>
  filter (congress %in% c(87, 91, 95, 
                          99, 103, 107, 
                          111, 115, 119)) |>
  
  ggplot(aes(x = nominate_dim1, 
             y = nominate_dim2) ) +
  
          annotate("path",
               x=cos(seq(0,2*pi,length.out=300)),
               y=sin(seq(0,2*pi,length.out=300)),
               color='gray',
               size = .25) +
  
  geom_point(aes(color = Member), 
             size= 1.25,
             shape = 17) + 
  
  scale_color_manual(values = c('#1a476f', '#8faabe',
                                '#e19463', '#913a40')) +
  
  facet_wrap(~year + congress) +
  theme_minimal() +
  theme(legend.title=element_blank(),
        legend.position = 'bottom') +
  labs(title="The evolution of the Southern Republican",
       subtitle = 'In two dimensions: from 1959 to 2023',
       caption = 'Data source: VoteView')
```

![](README_files/figure-markdown_github/unnamed-chunk-32-1.png)

------------------------------------------------------------------------

``` r
gens <- read.csv('https://raw.githubusercontent.com/jaytimm/AmericanGenerations/main/data/pew-plus-strauss-generations.csv') |>
  mutate(order = row_number()) |>
  filter(order %in% c(5:19))
```

## Age, generations & freshman classes in the House

### Average age of House members

``` r
congress |>
  mutate(age = year - born) |>
  filter (party_code %in% c('100', '200')) |>
  #filter(year > 1960) |>
  group_by(party_name, year) |>
  summarize(age = round(mean(age, na.rm = T), 1)) |>
  mutate(label = if_else(year == max(year) | year == min(year), 
                         age, NA_real_)) |>
  
  ggplot() +
  geom_line(aes(x = year + 1, 
                y = age, 
                color = party_name), 
            size = .8) +
  
    ggrepel::geom_text_repel(aes(x = year + 1, 
                                 y = age, 
                                 label = label),
                             size= 3.25,
                             nudge_x = 1,
                             na.rm = TRUE) +

  ggthemes::scale_color_stata()+
  theme_minimal() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x=element_blank()) +
  
  scale_x_continuous(breaks=seq(1921, 2025, 4)) +
  labs(title = "Average age of congress members by party",
       caption = 'Data source: VoteView') 
```

![](README_files/figure-markdown_github/unnamed-chunk-34-1.png)

### Introducing Generation Z

``` r
freshmen <- congress |>
  group_by(icpsr, bioname) |>
  mutate(n = length(congress)) |>
  ungroup() |>
  filter(congress == 119) |> # only correct here -- not older congresses
  
  mutate (Class = case_when (n == 1 ~ 'Freshman',
                             n == 2 ~ 'Sophmore',
                             n > 2 ~ 'Upper-class')) |>
  
  select(icpsr, party_code, Class)

gens1 <- gens |>
  mutate(age = 2026 - as.integer(end)) |>
  filter(order %in% c(15:19))
```

``` r
congress |>
  filter (party_code %in% c('100', '200'), 
          congress == 119) |> 
  mutate(age = year - born,
         party_code = ifelse(party_code == '100', 
                             'House Democrats', 
                             'House Republicans')) |>
  left_join(freshmen |> select(-party_code), by = "icpsr") |>
  
  ## 100 == democrat --
  ggplot() +
  
  geom_dotplot(aes(x = age, 
                   color = Class,
                   fill = Class),
               method="histodot",
               dotsize = .9, 
               binpositions = 'all', 
               stackratio = 1.3, 
               stackgroups=TRUE,
               binwidth = 1) + 
  
  geom_vline(xintercept =gens1$age - 0.5,
             linetype =2, 
             color = 'black', 
             size = .25) +
  
  geom_text(data = gens1, 
            aes(x = age + 2.25, 
                y = 0.95,
                label = generation),
            size = 3) +
  
  theme_minimal() + 
  ggthemes::scale_fill_economist() +
  ggthemes::scale_color_economist() +
  scale_x_continuous(breaks = gens1$age) +
  
  facet_wrap(~party_code, nrow = 2) +
  theme(legend.position = "bottom",
        axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  #ylim (0, .5) +
  
  labs(title = "Age distribution of the 119th House by party, generation & class",
       caption = 'Data sources: VoteView & AmericanGenerations')
```

![](README_files/figure-markdown_github/unnamed-chunk-36-1.png)

### First-timers in the House

``` r
freshmen1 <- congress |>
  group_by(icpsr, bioname, party_name) |>
  summarize(min = min(year),
            max = max(year)) |>
  group_by(min, party_name) |>
  summarise(count = n()) |>
  ungroup() |>
  filter(min > 1960, party_name != 'other')
  

labs <- freshmen1 |>
  arrange(desc(min)) |>
  top_n(4, count) |>
  mutate(txt = c('Obama 1st midterm',  
                 'Clinton 1st midterm', 
                 '"Watergate babies"', 
                 'LBJ atop ticket'))

freshmen1 |>
  ggplot() +
  geom_line(aes(x = min + 1, 
                y = count, 
                color = party_name),
            size = 0.8) +
  
  geom_text(data = labs,
            aes(x = min, 
                y = count, 
                label = txt),
            size = 3, nudge_y = 3) +
  ggthemes::scale_color_stata()+
  theme_minimal() +
  theme(legend.position = 'none',
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  scale_x_continuous(breaks=seq(1963,2025,2)) +
  labs(title = "Freshman House members by party",
       caption = 'Data source: VoteView')
```

![](README_files/figure-markdown_github/unnamed-chunk-37-1.png)

------------------------------------------------------------------------

## Towards 2026

### Class II Senators

``` r
class1 <- states |> filter(!state_code %in% nonx[1:5]) |>
  left_join(leg_dets0 |> filter(senate_class == 2)) |>
  left_join(PresElectionResults::pres_by_state |>
              filter(year == 2024)) |>
  mutate(bmar = round(democrat - republican, 1),
         bmar = ifelse(bmar < 0, paste0('(', gsub('-', '', bmar), ')'), bmar)) |>
  mutate(label = ifelse(is.na(senate_class), state_abbrev,
                        paste0(state_abbrev, 
                        '\n', 
                        last_name,
                        '\n',
                        bmar
                        ))) |>
  mutate(party = ifelse(is.na(party), 'x-Class I', party)) |>
  mutate(party = as.factor(party)) |>
  mutate(party = forcats::fct_relevel(party, 
                                       'Independent', 
                                        after = 2))
```

``` r
new2 <- PresElectionResults::xsf_TileInv10 |> 
  left_join(class1)


PresElectionResults::xsf_TileOutv10 |> 
  left_join(class1, by ='state_abbrev') |>
  ggplot() + 
  geom_sf(aes(fill = party),
          color = 'darkgray' , 
          alpha = .65) + 
  
  geom_sf_text(data = new2,
                          aes(label = label), 
                          size = 2.75,
                          color = 'black') +

  theme_minimal() + 
  theme_guide() + 
  #theme(legend.position = 'none') +
  ggthemes::scale_fill_stata()+
  #scale_fill_brewer(palette = 'YlGnBu') +
  labs(title = "Class II Senators, 2026",
       subtitle = 'With 2024 Harris margins',
       caption = 'Data sources: unitedstates.io, Wikipedia & Daily Kos')
```

![](README_files/figure-markdown_github/unnamed-chunk-39-1.png)

### Vulnerable Republican House Members

> House Republicans in 119th representing districts Harris won in 2024.

> Note: There are a few new re-worked districts in California from
> mid-census redistricting that are not included here.

``` r
vrs <- PresElectionResults::pres_by_cd |>
  filter(house_rep_party == 'republican',
         party_win == 'democrat') |>
  left_join(freshmen) |>
  mutate(Harris_Margin = democrat - republican,
         district = paste0(state_abbrev, '-', district_code)) |>
  
  select(district, house_rep, Class, Harris_Margin) |> 
  arrange(-Harris_Margin)
 
vrs |> knitr::kable()
```

| district | house_rep            | Class       | Harris_Margin |
|:---------|:---------------------|:------------|--------------:|
| NE-02    | Don Bacon            | Upper-class |          4.62 |
| NY-17    | Michael Lawler       | Sophmore    |          0.56 |
| PA-01    | Brian K. Fitzpatrick | Upper-class |          0.32 |

## Fin
