# STA141B Project - Shiny App
STA141B Project. Creation of Shiny app with API implementation. URL: https://alvis-ip.shinyapps.io/sta141bproject/

## Overview
This app pulls resources from the Alpha Vantage API (https://www.alphavantage.co/) and generates stock graphs with simple analyses upon request. Users are able to fetch daily, weekly, or monthly charts, and can review supplemental analysis on the company.

## Graph Specifications
Upon request of a frequency, a chart is generated with two additional trendlines. Displayed lines will include the company price history, a 10-interval [simple moving average](https://www.investopedia.com/terms/s/sma.asp#:~:text=A%20simple%20moving%20average%20(SMA)%20is%20an%20arithmetic%20moving%20average,periods%20in%20the%20calculation%20average.), and a 10-interval [exponential moving average](https://www.investopedia.com/terms/e/ema.asp#:~:text=An%20exponential%20moving%20average%20(EMA)%20is%20a%20type%20of%20moving,the%20exponentially%20weighted%20moving%20average.). 

Notes:
- Intervals are based upon the frequency of the graph; e.g. selecting "Daily" will yield a 10-day moving average.
- Moving averages are calculated on 20 data points by default.
- Graphs and MAs will only use opening values, if applicable.

## Analysis Specifications
Four analysis options are available to the user, being:

- **Valuation**: Provides an overall evaluation based upon various technical parameters. Values stocks are either [underweight](https://www.investopedia.com/terms/u/underweight.asp), [equal weight](https://www.investopedia.com/terms/e/equalweight.asp), or [overweight](https://www.investopedia.com/terms/o/overweight.asp).
- **Weekly Performance**: Gives user low and high values of weekly performance of company. Given its intraweek swings, the app determines if the company in the short term has experienced low, medium, or high [volatility](https://www.investopedia.com/terms/v/volatility.asp).
- **Technical Analysis**: Tells user if the current price of the stock is above, at, or below its moving average. Given this, it also marks potential areas of [support](https://www.investopedia.com/terms/s/support.asp) or [resistance](https://www.investopedia.com/terms/r/resistance.asp) the stock may face in the short term.
- **Volume/Liquidity**: Returns the average number of shares traded by the stock each day over the past week of active trading. Determines if the stock is high, medium, or low [volume](https://www.investopedia.com/terms/a/averagedailytradingvolume.asp) and if it is highly [liquid](https://www.investopedia.com/terms/l/liquidityratios.asp), somewhat liquid, or illiquid.

## Restrictions
- Not all securities are supported by Alpha Vantage API. Expect that some NASDAQ securities will not be available.
- Alpha Vantage API only allows for 5 requests per minute. Generation of the graph/analysis may result in overloading of requests if done too quickly, so you may only be able to generate 1 graph per minute.
