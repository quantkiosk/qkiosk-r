<img src="https://quantkiosk.com/assets/img/qk-logo.png" height="40" />

>[!IMPORTANT]
> **FREE API keys are now enabled for all accounts. Get yours [here](https://quantkiosk.com?ref=github-qkiosk-r)!!**
>
> Paid plans will be open in the coming weeks as we finish backfill and add more features!

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
># Data sets in package ‘qkiosk’:
>#
># crox        Crocs Institutional Holders Details By Issuer
># deshaw      D.E. Shaw Institutional Ownership Details (Including Submanagers)
># nke         Nike (NKE) Insider Ownership Data
># pershing    Pershing Square Beneficial and Activist Details
># pfe         Pfizer (PFE) Revenue Data
># sgcap       SG Capital Institutional Ownership Details (Aggregated)

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
qk_ticker("AAPL")
#                      AAPL 
# 0000320193.0000.001S5N8V8

to_name(qk_cik(78003))
# [1] "PFIZER INC"
to_permid(qk_cik(78003))
# [1] "4295904722"

# use fuzzy search for a company like Alibaba
BABA <- qk_search_co("alibaba")
#
# 1: ALIBABA GROUP HOLDING-SP ADR
# 2: ALITHYA USA INC
# 3: ALIGNMENT HEALTHCARE INC
# 4: ALTI GLOBAL INC
# 5: ALLY FINANCIAL INC
# 6: ALLEGIANT TRAVEL CO
#
#Selection: 1
BABA
# ALIBABA GROUP HOLDING-SP ADR 
#    0001577552.0400.006G2JWB1
```

You can also search for fund managers in a similar way
```
janest <- qk_search_mgr("jane street")

# 1: JANE STREET GROUP LLC
# 2: JANNEY MONTGOMERY SCOTT LLC
# 3: JANA PARTNERS MANAGEMENT LP
# 4: JANNEY CAPITAL MANAGEMENT LLC
# 5: JOURNEY STRATEGIC WEALTH LLC
# 6: JAMES INVESTMENT RESEARCH INC

# Selection: 1

janest
#     JANE STREET GROUP LLC 
# 0001595888.0000.E0000Y7E8 
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
> BABA
> # ALIBABA GROUP HOLDING-SP ADR 
> # 0001577552.0400.006G2JWB1
>```
> - *[ENTITY]* the 10 digit CIK assigned by the SEC to any entity that files in the US. Even foreign firms need one at times.
> - *[CLS]* is the classification of the instrument. In this case the 0400 is an ADR tradable in the US. 0000 is used for the common equity - often Class A Ordinary, and there are many others available
> - *[INSTRUMENT]* is the variable portion of the FIGI from OpenFIGI, which is quickly becoming the de-facto identifier - covering a billion+ instruments
>
>For entities that do not have instruments (e.g. a hedge fund or a CEO), we create a unique instrument part to allow for consistency (e.g. **E0000Y7E8** like in Jane Street's case above).

## Get some data.

QK's job is to let you do _your_ job. Mapped, point-in-time, and even auditable - all from within your preferred platform - is how we make that happen. 
To get a feel for this, we'll take a look at two foundational areas - **Ownership** and **Fundamentals**:

- *Universe*
- *Institutional Ownership*
- *Insider Ownership*
- *Activist Ownership*
- *Fundamentals*

Let's get started

### Start with a Universe

In all institutional settings, you are almost always working with a universe. This is nothing more than a collection
of firms or instruments that are part of a strategy. These are often defined by a set of rules - e.g. minimum market cap and
price, some limits on minimum daily volume, etc.

To get you started, we have created a set of universe definitions to showcase the design and data behind **QUANTkiosk**. We'll elaborate on methodologies at another time,
but these represent the most widely held companies amongst institutional investors. From the top 100 to 3000 firms (**QK100**, **QK1000**, **QK3000**, with **QK2000** being those firms less widley held than
the top 1000)

```
qk100 <- qk_univ("QK100")
head(qk100)
# [1] 0000320193.0000.001S5N8V8 0001018724.0000.001S5PQL7 0001652044.0000.009S3NB21 0001045810.0000.001S5TZJ6 0001326801.0000.001SQCQC5 0000019617.0000.001S8CRC3

head(to_ticker(qk100))
#[1] "AAPL" "AMZN" "GOOG" "NVDA" "META" "JPM" 

head(to_name(qk100))
# [1] "APPLE INC"                  "AMAZON.COM INC"             "ALPHABET INC-CL C"          "NVIDIA CORP"                "META PLATFORMS INC-CLASS A" "JPMORGAN CHASE & CO" 
```

Of course, not everything you might want to request is a tradable entity.  In fact, the _owners_ of the above universe are private firms in many cases.

### Institutional Ownership

What they call "smart money". Certain asset managers (those with over $100mm in reportable assets) are obliged to disclose their positions at the end of each quarter. The `qkiosk` package includes two
examples - `deshaw` and `sgcap`, both which provide good documentation on what the structure of the data is. For our purposes, lets just request some data to see how easy it is to get a picture of what held last quarter.

First, well get the manager that became particularly popular in the pandemic. The are a very large multistrat player, who reports for both the asset manager business and the securities business. To get a good feel for
what they are doing for the investing side, it is very useful to be able to see submanager details, which QK does very well.

```
citadel <- qk_search_mgr("citadel")
#  1: CITADEL ADVISORS LLC
#  2: CITADEL INVESTMENT ADVISORY INC.
#  3: CIT BANK NA WEALTH MANAGEMENT
#  4: CITY STATE BANK
#  5: CIC WEALTH LLC
#  6: CITIZENS FINANCIAL GROUP INC/RI
#  7: CITY CENTER ADVISORS LLC
#  8: CITY HOLDING CO
#  9: CITIZENS NATIONAL BANK TRUST DEPARTMENT
# 10: CI PRIVATE WEALTH LLC
# Selection: 1

citadel
#      CITADEL ADVISORS LLC 
# 0001423053.0000.E0000UI19
```
Now that we have our QKID, we can use it in the function called `qk_institutional`. Interface matters a lot to us. If you don't notice how easy it is to use, it is because we've done our job.
```
ca_202402_202501 <- qk_institutional(citadel, yyyyqq=202501, qtrs=4, agg=FALSE)
# fetching 0001423053 for 202501 ...done.
# fetching 0001423053 for 202404 ...done.
# fetching 0001423053 for 202403 ...done.
# fetching 0001423053 for 202402 ...done.
```

![image](https://github.com/user-attachments/assets/ec01fa81-021b-4389-bbc0-84061f6b7590)

It's easy to see what Citadel Securities is holding, how much it has changed, and even see positions they no longer have.
```
ca_202402_202501 |>
  subset(otherManager==1,
         select=c('reportPeriod','issuer','putCall','issuerQkid','issuerSector','value','QOQSshPrnAmt','otherManagerName','newOrDel')) 

```
![image](https://github.com/user-attachments/assets/3206dd38-bfc3-4e34-97ea-38f2068de8f4)

There are of course, a million ways to use this data - and we promise to share videos as well as deep dives as we move forward. We also would love to have anyone using the API share how they 
are discovering insights into what the "Who's Who of Wall Street" are up to each quarter.

We can also explore details of company holders, large shareholders, and even corporate insiders with similar speed and ease.

```
# all holders of NVDA in 2022 Q1
nvda_holders <- qk_holders(qk_ticker("NVDA"), yyyyqq=202201)

# NVDA insiders - (e.g. CEO, Sr.EVP, ...)
nvda_insiders <- qk_insider(qk_ticker("NVDA"), yyyyqq=202201)

# Large NVDA block holders for 2022 (above 5%)
nvda_lg <- qk_beneficial(qk_ticker("NVDA"), yyyyqq=202200)
```

### Fundamentals

Fundamentals data is the lifeblood of a company. While prices are observable, financials are much harder to source
consistently or correctly.  We source directly from filings, in a way only people who have used this data for
investment at scale know how to do.

Let's dig into a quick example to show off what you can do with a few lines of code.
```
# There are hundreds of line items that are generally used - though 10s of 1000s of GAAP items are available.
#
# To see common ones we currently map you can use our reference function qk_fncodes
qk_fncodes()
```
<img width="621" alt="image" src="https://github.com/user-attachments/assets/3993d057-bcaf-4a1d-a696-aaf0931e6398" />

From here, lets find Net Income (code NI) for UBER

```
uber_ni <- qk_fn(qk_ticker("UBER"), "NI")
```
<img width="1051" alt="image" src="https://github.com/user-attachments/assets/06d42814-99dc-4ba7-96de-ea72166bbb93" />

You can also easily see where this data comes from using our state of the art auditing tools
```
qk_fn(qk_ticker("UBER"), "NI") |> to_df() |> tail() |> highlight(5)

qk_fn(qk_ticker("UBER"), "NI") |> to_df() |> tail() |> highlight(5) |> qk_audit()
```
<img width="1022" alt="image" src="https://github.com/user-attachments/assets/3afa85af-168c-4c09-a5f7-8e13635e2d83" />

<img width="1119" alt="image" src="https://github.com/user-attachments/assets/903a6d09-9f5d-416b-a484-449a7504ddc3" />






