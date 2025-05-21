<img src="https://quantkiosk.com/assets/img/qk-logo.png" height="40" />

>[!IMPORTANT]
> **API keys will be rolled out to those signed up in the order they sign up. Join the queue [here](https://quantkiosk.com?ref=github-qkiosk-r)!!**

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
**FREE** key go to the [account page](https://www.quantkiosk.com/account). If you have not signed up for an account, you can
enter your email and your account key will be good for 250 credits a day. More than enough to explore and
make use of the API. If you need more data, just select an appropriate plan.

```r
## Set your API key in the R session after you've load the freshly installed package

require(qkiosk)
qk_set_apikey("<YOUR_API_KEY>")
```

>[!TIP]
>```bash
>## you can also set up your key in your shell to avoid having to set it in R
>## this is more permanent and definitely how you would do it in production
>
>export QK_API_KEY=<YOUR_API_KEY>
>```

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

### Symbology drives everything.

The most important part of any institutional data is getting the mappings correct. The best hedge funds take having a security master for granted. It is also
an incredible pain point that firms spend millions a year to manage, or tens of thousands (or more!) to buy.

Once you have access to a proper symbology - you won't understand how you ever lived without it.

**QUANTkiosk** is solving this once and for all by creating an open *entity and security* master to help
leverage our data with as little effort as possible.  You can read more about our project on the site, but in short, everything
within the API is referenced by a **QKID**. To make that _easy_ we have some helper functions. Both search and conversion
is part of the batteries included mindset of QK.

Obviously you likely have _some_ identifier to begin with. Most often this is a ticker, like **AAPL** or **MRK**. You can use this
to map to a **QKID**, but you can even search right from your `R` session. This is especially useful for things that
don't have ticker - e.g. a hedge fund you want to track.

```
# You know the ticker or cik? Just use direct conversion:
> qk_ticker("AAPL")
                     AAPL 
0000320193.0000.001S5N8V8

> to_name(qk_cik(78003))
[1] "PFIZER INC"
> to_permid(qk_cik(78003))
[1] "4295904722"

# use fuzzy search for a company like Alibaba
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
>
>Alibaba will serve as an example of what this looks like in practice
>```
>> BABA
>  ALIBABA GROUP HOLDING-SP ADR 
>  0001577552.0400.006G2JWB1
>```
> - *[ENTITY]* the 10 digit CIK assigned by the SEC to any entity that files in the US. Even foreign firms need one at times.
> - *[CLS]* is the classification of the instrument. In this case the 0400 is an ADR tradable in the US. 0000 is used for the common equity - often Class A Ordinary, and there are many others available
> - *[INSTRUMENT]* is the variable portion of the FIGI from OpenFIGI, which is quickly becoming the de-facto identifier - covering a billion+ instruments
>
>For entities that do not have instruments (e.g. a hedge fund or a CEO), we create a unique identifier such as **E0000Y7E8** like in Jane Street's case above.

## Get some data.

QK's job is to let you do _your_ job. Mapped, point-in-time, and even auditable - all from within your preferred platform - is how we make that happen. 
To get a feel for this, we'll take a look at two foundational data:

- *Institutional Ownership*
- *Fundamentals*

Let's get started

### Institutional Ownership
```

```

### Fundamantals

```
```




