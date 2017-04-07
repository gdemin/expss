# Мой манускрипт
Gregory Demin  
2017.03.22  



## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```
## Loading required package: magrittr
```

```
## Loading required package: htmlTable
```

<table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;' >
<thead>
<tr>
<th style='border-top: 2px solid grey;'></th>
<th colspan='5' style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>&nbsp;Transmission&nbsp;</th>
</tr>
<tr>
<th style=''></th>
<th colspan='2' style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'>&nbsp;Automatic&nbsp;</th><th style='; border-bottom: hidden;'>&nbsp;</th>
<th colspan='2' style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'>&nbsp;Manual&nbsp;</th>
</tr>
<tr>
<th style=''></th>
<th colspan='2' style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'>&nbsp;Engine&nbsp;</th><th style='; border-bottom: hidden;'>&nbsp;</th>
<th colspan='2' style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'>&nbsp;Engine&nbsp;</th>
</tr>
<tr>
<th style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'></th>
<th style='border-bottom: 1px solid grey; text-align: center;'>&nbsp;V-engine&nbsp;</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>&nbsp;Straight engine&nbsp;</th>
<th style='border-bottom: 1px solid grey;' colspan='1'>&nbsp;</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>&nbsp;V-engine&nbsp;</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>&nbsp;Straight engine&nbsp;</th>
</tr>
</thead>
<tbody> 
<tr><td colspan='6' style='font-weight: 900;'>&nbsp;Miles/(US) gallon&nbsp;</td></tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;</td>
<td style='text-align: right;'>15.1</td>
<td style='text-align: right;'>20.7</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'>19.8</td>
<td style='text-align: right;'>28.4</td>
</tr> 
<tr><td colspan='6' style='font-weight: 900;'>&nbsp;Gross horsepower&nbsp;</td></tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;</td>
<td style='text-align: right;'>194.2</td>
<td style='text-align: right;'>102.1</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'>180.8</td>
<td style='text-align: right;'>80.6</td>
</tr> 
<tr><td colspan='6' style='font-weight: 900;'>&nbsp;Transmission&nbsp;</td></tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;&nbsp;Automatic&nbsp;</td>
<td style='text-align: right;'>100</td>
<td style='text-align: right;'>100</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'></td>
<td style='text-align: right;'></td>
</tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;&nbsp;Manual&nbsp;</td>
<td style='text-align: right;'></td>
<td style='text-align: right;'></td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'>100</td>
<td style='text-align: right;'>100</td>
</tr>
<tr>
<td style='border-bottom: 2px solid grey; text-align: left;'>&nbsp;&nbsp;&nbsp;#Total cases&nbsp;</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>12</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>7</td>
<td style='border-bottom: 2px solid grey;' colspan='1'>&nbsp;</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>6</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>7</td>
</tr>
</tbody>
</table><table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;' >
<thead>
<tr>
<th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>"Моя русская метка"</th>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>&nbsp;Count&nbsp;</th>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>&nbsp;Valid percent&nbsp;</th>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>&nbsp;Percent&nbsp;</th>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>&nbsp;Responses, %&nbsp;</th>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>&nbsp;Cumulative responses, %&nbsp;</th>
</tr>
</thead>
<tbody>
<tr>
<td style='text-align: left;'>&nbsp;Моя русская метка&nbsp;</td>
<td style='text-align: right;'>1</td>
<td style='text-align: right;'>100</td>
<td style='text-align: right;'>100</td>
<td style='text-align: right;'>100</td>
<td style='text-align: right;'>100</td>
</tr>
<tr>
<td style='text-align: left;'>&nbsp;#Total&nbsp;</td>
<td style='text-align: right;'>1</td>
<td style='text-align: right;'>100</td>
<td style='text-align: right;'>100</td>
<td style='text-align: right;'>100</td>
<td style='text-align: right;'></td>
</tr>
<tr>
<td style='border-bottom: 2px solid grey; text-align: left;'>&nbsp;<NA>&nbsp;</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>0</td>
<td style='border-bottom: 2px solid grey; text-align: right;'></td>
<td style='border-bottom: 2px solid grey; text-align: right;'>0</td>
<td style='border-bottom: 2px solid grey; text-align: right;'></td>
<td style='border-bottom: 2px solid grey; text-align: right;'></td>
</tr>
</tbody>
</table><table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;' >
<thead>
<tr>
<th style='border-top: 2px solid grey;'></th>
<th colspan='2' style='font-weight: 900; border-top: 2px solid grey; text-align: center;'></th><th style='border-top: 2px solid grey;; border-bottom: hidden;'>&nbsp;</th>
<th colspan='2' style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>&nbsp;Engine&nbsp;</th>
</tr>
<tr>
<th style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'></th>
<th style='border-bottom: 1px solid grey; text-align: center;'></th>
<th style='border-bottom: 1px solid grey; text-align: center;'></th>
<th style='border-bottom: 1px solid grey;' colspan='1'>&nbsp;</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>&nbsp;V-engine&nbsp;</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>&nbsp;Straight engine&nbsp;</th>
</tr>
</thead>
<tbody> 
<tr><td colspan='6' style='font-weight: 900;'>&nbsp;Engine&nbsp;</td></tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;&nbsp;V-engine&nbsp;</td>
<td style='text-align: left;'>&nbsp;Transmission&nbsp;</td>
<td style='text-align: left;'>&nbsp;Automatic&nbsp;</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'>66.7</td>
<td style='text-align: right;'></td>
</tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;</td>
<td style='text-align: left;'></td>
<td style='text-align: left;'>&nbsp;Manual&nbsp;</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'>33.3</td>
<td style='text-align: right;'></td>
</tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;&nbsp;Straight engine&nbsp;</td>
<td style='text-align: left;'>&nbsp;Transmission&nbsp;</td>
<td style='text-align: left;'>&nbsp;Automatic&nbsp;</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'></td>
<td style='text-align: right;'>50</td>
</tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;</td>
<td style='text-align: left;'></td>
<td style='text-align: left;'>&nbsp;Manual&nbsp;</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'></td>
<td style='text-align: right;'>50</td>
</tr> 
<tr><td colspan='6' style='font-weight: 900;'>&nbsp;#Total cases&nbsp;</td></tr>
<tr>
<td style='border-bottom: 2px solid grey; text-align: left;'>&nbsp;&nbsp;</td>
<td style='border-bottom: 2px solid grey; text-align: left;'></td>
<td style='border-bottom: 2px solid grey; text-align: left;'></td>
<td style='border-bottom: 2px solid grey;' colspan='1'>&nbsp;</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>18</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>14</td>
</tr>
</tbody>
</table><table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;' >
<thead>
<tr>
<th style='border-top: 2px solid grey;'></th>
<th colspan='2' style='font-weight: 900; border-top: 2px solid grey; text-align: center;'></th><th style='border-top: 2px solid grey;; border-bottom: hidden;'>&nbsp;</th>
<th colspan='2' style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>&nbsp;Engine&nbsp;</th>
</tr>
<tr>
<th style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'></th>
<th style='border-bottom: 1px solid grey; text-align: center;'></th>
<th style='border-bottom: 1px solid grey; text-align: center;'></th>
<th style='border-bottom: 1px solid grey;' colspan='1'>&nbsp;</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>&nbsp;V-engine&nbsp;</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>&nbsp;Straight engine&nbsp;</th>
</tr>
</thead>
<tbody> 
<tr><td colspan='6' style='font-weight: 900;'>&nbsp;Engine&nbsp;</td></tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;&nbsp;V-engine&nbsp;</td>
<td style='text-align: left;'>&nbsp;Transmission&nbsp;</td>
<td style='text-align: left;'>&nbsp;Automatic&nbsp;</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'>66.7</td>
<td style='text-align: right;'></td>
</tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;</td>
<td style='text-align: left;'></td>
<td style='text-align: left;'>&nbsp;Manual&nbsp;</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'>33.3</td>
<td style='text-align: right;'></td>
</tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;&nbsp;Straight engine&nbsp;</td>
<td style='text-align: left;'>&nbsp;Transmission&nbsp;</td>
<td style='text-align: left;'>&nbsp;Automatic&nbsp;</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'></td>
<td style='text-align: right;'>50</td>
</tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;</td>
<td style='text-align: left;'></td>
<td style='text-align: left;'>&nbsp;Manual&nbsp;</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'></td>
<td style='text-align: right;'>50</td>
</tr> 
<tr><td colspan='6' style='font-weight: 900;'>&nbsp;#Total cases&nbsp;</td></tr>
<tr>
<td style='border-bottom: 2px solid grey; text-align: left;'>&nbsp;&nbsp;</td>
<td style='border-bottom: 2px solid grey; text-align: left;'></td>
<td style='border-bottom: 2px solid grey; text-align: left;'></td>
<td style='border-bottom: 2px solid grey;' colspan='1'>&nbsp;</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>18</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>14</td>
</tr>
</tbody>
</table>***Мой заголовок 1***<table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;' >
<thead>
<tr>
<th style='border-top: 2px solid grey;'></th>
<th colspan='5' style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>&nbsp;Engine&nbsp;</th>
</tr>
<tr>
<th style=''></th>
<th colspan='2' style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'>&nbsp;V-engine&nbsp;</th><th style='; border-bottom: hidden;'>&nbsp;</th>
<th colspan='2' style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'>&nbsp;Straight engine&nbsp;</th>
</tr>
<tr>
<th style=''></th>
<th colspan='2' style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'>&nbsp;Transmission&nbsp;</th><th style='; border-bottom: hidden;'>&nbsp;</th>
<th colspan='2' style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'>&nbsp;Transmission&nbsp;</th>
</tr>
<tr>
<th style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'></th>
<th style='border-bottom: 1px solid grey; text-align: center;'>&nbsp;Automatic&nbsp;</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>&nbsp;Manual&nbsp;</th>
<th style='border-bottom: 1px solid grey;' colspan='1'>&nbsp;</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>&nbsp;Automatic&nbsp;</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>&nbsp;Manual&nbsp;</th>
</tr>
</thead>
<tbody> 
<tr><td colspan='6' style='font-weight: 900;'>&nbsp;Engine&nbsp;</td></tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;&nbsp;V-engine&nbsp;</td>
<td style='text-align: right;'>100</td>
<td style='text-align: right;'>100</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'></td>
<td style='text-align: right;'></td>
</tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;&nbsp;Straight engine&nbsp;</td>
<td style='text-align: right;'></td>
<td style='text-align: right;'></td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'>100</td>
<td style='text-align: right;'>100</td>
</tr>
<tr>
<td style='border-bottom: 2px solid grey; text-align: left;'>&nbsp;&nbsp;&nbsp;#Total cases&nbsp;</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>12</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>6</td>
<td style='border-bottom: 2px solid grey;' colspan='1'>&nbsp;</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>7</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>7</td>
</tr>
</tbody>
</table><br><br><br>***Мой заголовок 2***<!--html_preserve--><table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;' >
<thead>
<tr><td colspan='9' style='text-align: left;'>
<b><i>Таблица 3</i></b></td></tr>
<tr>
<th style='border-top: 2px solid grey;'></th>
<th colspan='2' style='font-weight: 900; border-top: 2px solid grey; text-align: center;'></th><th style='border-top: 2px solid grey;; border-bottom: hidden;'>&nbsp;</th>
<th colspan='5' style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>&nbsp;Engine&nbsp;</th>
</tr>
<tr>
<th style=''></th>
<th colspan='2' style='font-weight: 900; text-align: center;'></th><th style='; border-bottom: hidden;'>&nbsp;</th>
<th colspan='2' style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'>&nbsp;V-engine&nbsp;</th><th style='; border-bottom: hidden;'>&nbsp;</th>
<th colspan='2' style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'>&nbsp;Straight engine&nbsp;</th>
</tr>
<tr>
<th style=''></th>
<th colspan='2' style='font-weight: 900; text-align: center;'></th><th style='; border-bottom: hidden;'>&nbsp;</th>
<th colspan='2' style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'>&nbsp;Transmission&nbsp;</th><th style='; border-bottom: hidden;'>&nbsp;</th>
<th colspan='2' style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'>&nbsp;Transmission&nbsp;</th>
</tr>
<tr>
<th style='font-weight: 900; border-bottom: 1px solid grey; text-align: center;'></th>
<th style='border-bottom: 1px solid grey; text-align: center;'></th>
<th style='border-bottom: 1px solid grey; text-align: center;'></th>
<th style='border-bottom: 1px solid grey;' colspan='1'>&nbsp;</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>&nbsp;Automatic&nbsp;</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>&nbsp;Manual&nbsp;</th>
<th style='border-bottom: 1px solid grey;' colspan='1'>&nbsp;</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>&nbsp;Automatic&nbsp;</th>
<th style='border-bottom: 1px solid grey; text-align: center;'>&nbsp;Manual&nbsp;</th>
</tr>
</thead>
<tbody> 
<tr><td colspan='9' style='font-weight: 900;'>&nbsp;Engine&nbsp;</td></tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;&nbsp;V-engine&nbsp;</td>
<td style='text-align: left;'>&nbsp;Transmission&nbsp;</td>
<td style='text-align: left;'>&nbsp;Automatic&nbsp;</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'>100</td>
<td style='text-align: right;'></td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'></td>
<td style='text-align: right;'></td>
</tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;</td>
<td style='text-align: left;'></td>
<td style='text-align: left;'>&nbsp;Manual&nbsp;</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'></td>
<td style='text-align: right;'>100</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'></td>
<td style='text-align: right;'></td>
</tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;&nbsp;Straight engine&nbsp;</td>
<td style='text-align: left;'>&nbsp;Transmission&nbsp;</td>
<td style='text-align: left;'>&nbsp;Automatic&nbsp;</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'></td>
<td style='text-align: right;'></td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'>100</td>
<td style='text-align: right;'></td>
</tr>
<tr>
<td style='text-align: left;'>&nbsp;&nbsp;</td>
<td style='text-align: left;'></td>
<td style='text-align: left;'>&nbsp;Manual&nbsp;</td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'></td>
<td style='text-align: right;'></td>
<td style='' colspan='1'>&nbsp;</td>
<td style='text-align: right;'></td>
<td style='text-align: right;'>100</td>
</tr> 
<tr><td colspan='9' style='font-weight: 900;'>&nbsp;#Total cases&nbsp;</td></tr>
<tr>
<td style='border-bottom: 2px solid grey; text-align: left;'>&nbsp;&nbsp;</td>
<td style='border-bottom: 2px solid grey; text-align: left;'></td>
<td style='border-bottom: 2px solid grey; text-align: left;'></td>
<td style='border-bottom: 2px solid grey;' colspan='1'>&nbsp;</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>12</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>6</td>
<td style='border-bottom: 2px solid grey;' colspan='1'>&nbsp;</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>7</td>
<td style='border-bottom: 2px solid grey; text-align: right;'>7</td>
</tr>
</tbody>
</table><!--/html_preserve-->

## Including Plots

You can also embed plots, for example:

![](test_knit_print_files/figure-html/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
