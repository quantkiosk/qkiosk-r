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
qk_set_apikey("<YOUR_API_KEY")
```
