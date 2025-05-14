<img src="https://quantkiosk.com/assets/img/qk-logo.png" height="40" />

## R Client

Official interface to [QUANTkiosk](https://quantkiosk.com) data api.

<img width="753" alt="image" src="https://github.com/user-attachments/assets/5d328e70-cb5a-4a8e-b0f2-2bf4d6de8d72" />

### Features
* A true extension of `R` - use the tools you are productive with.
* Integrated symbology to easily map entities and instruments.
* Limited dependencies to ensure easy installation and no conflicts.
* Access to all data endpoints, including Ownership and Fundamentals.
* Parallel API requests for large downloads in a fraction of the time.
* Internal caching to minimize external requests and data usage

### Installation
```r
# install remotes using install.packages("remotes")
require(remotes)

install_github("https://github.com/quantkiosk/qkiosk-r")
```

### Set up your API key
All access to live and historical data requires a valid `QK_API_KEY` to be set. To get your
key go to the [account page](https://www.quantkiosk.com/account). If you have not signed up for an account, you can
enter your email and your account key will be good for 250 credits a day. More than enough to explore and
make use of the API. If you need more data, just select an appropriate plan.

```bash
## bash shell

export QK_API_KEY=<YOUR_API_KEY>
```

```r
## R session

require(qkiosk)
qk_set_apikey("<YOUR_API_KEY>")
```

>[!TIP]
>You don't actually need a key to get started with the package. We've included datasets that represent what each of the endpoints
>return to help get a feel for the breadth and depth of what QK does.
>```
>data(package="qkiosk")
>Data sets in package ‘qkiosk’:
>
>crox        Crocs Institutional Holders Details By Issuer
>deshaw      D.E. Shaw Institutional Ownership Details (Including Submanagers)
>nke         Nike (NKE) Insider Ownership Data
>pershing    Pershing Square Beneficial and Activist Details
>pfe         Pfizer (PFE) Revenue Data
>sgcap       SG Capital Institutional Ownership Details (Aggregated)

## Get Started

### Symbology drives everything

The most important part of any institutional data is often its weakest link. The best hedge funds know that understanding what you are
looking at - be it an entity or instrument - is so critical it is just taken for granted amongst professionals. It is also
an incredible pain point that firms spend millions a year to manage.

Once you have access to one, you never want to go without.

QUANTkiosk is looking to solve this once and for all by creating an open *entity and security* master to help
leverage our data with as little effort as possible.  You can read more about our efforts on the site, but in short, everything
within the API is referenced by a **QKID**. To make that _easy_ we have some helper functions. Both search and conversions come out of the box

```
# search for a company like Alibaba
> BABA <- qk_search_co("alibaba")

 1: ALIBABA GROUP HOLDING-SP ADR
 2: ALITHYA USA INC
 3: ALIGNMENT HEALTHCARE INC
 4: ALTI GLOBAL INC
 5: ALLY FINANCIAL INC
 6: ALLEGIANT TRAVEL CO

Selection: 1
> BABA
ALIBABA GROUP HOLDING-SP ADR 
   0001577552.0400.006G2JWB1
```
You can also search for fund managers in a similar way
```
> janest <- qk_search_mgr("jane street")

 1: JANE STREET GROUP LLC
 2: JANNEY MONTGOMERY SCOTT LLC
 3: JANA PARTNERS MANAGEMENT LP
 4: JANNEY CAPITAL MANAGEMENT LLC
 5: JOURNEY STRATEGIC WEALTH LLC
 6: JAMES INVESTMENT RESEARCH INC

Selection: 1
> janest
    JANE STREET GROUP LLC 
0001595888.0000.E0000Y7E8 
> 

```

There is *way* more to know about the **QKID**, but this is the README, so we will move along.  Be sure to try conversion tools like `qk_ticker("AAPL")` and `to_ticker(qk_cik(320187))` to see the power
for yourself.

>[!NOTE]
>A **QKID** is actually a just a unique combination of entity, class and an instrument:
>
>  *[ENTITY].[CLS].[INSTRUMENT]*
>
>The entity is most often the **CIK**, the instrument is an OpenFIGI **FIGI**, and the class is something that helps to quickly identify what this
>instrument is.  Pretty simple, but unlike most everything you might have seen in a security master.


## Data API

QK presents data in a way that allows you to both understand its source, as well as abstract it to be able to do your job. Mapped, point-in-time, and even auditable - all from within the client. 
To get a feel for this, we'll take a look at one of the endpoints for *Ownership* data,
specifically *Institutional Ownership*. This comes from the regulatory filings of large and (often) sophisticated managers. Someone like a Jane Street will help .

```

```






