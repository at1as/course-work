### Usage

Work in Progress

```
$ erl
> c(index).
> c(helpers).
> c(search).
>
> index:runner().
>
```


### Output

#### Data Source 1

```
index:runner().
[{"Four",[{1,1}]},
 {"score",[{1,1}]},
 {"and",[{1,1},{3,3},{6,6},{10,10},{15,15},{27,27}]},
 {"seven",[{1,1}]},
 {"years",[{1,1}]},
 {"ago",[{1,1}]},
 {"our",[{1,1},{16,16}]},
 {"fathers",[{1,1}]},
 {"brought",[{1,1}]},
 {"forth",[{2,2}]},
 {"on",[{2,2},{7,7}]},
 {"this",[{2,2},{14,14},{26,26}]},
 {"continent,",[{2,2}]},
 {"a",[{2,2},{5,5},{7,8},{13,13},{26,26}]},
 {"new",[{2,2},{26,26}]},
 {"nation,",[{2,2},{6,6},{26,26}]},
 {"conceived",[{2,2},{6,6}]},
 {"in",[{2,2},{5,5},{13,13},{25,25}]},
 {"Liberty,",[{2,2}]},
 {"dedicated",[{3,3},{19,19},{21,21}]},
 {"to",[{3,3},{8,8},{16,16},{19,19},{21,...},{...}]},
 {"the",[{3,3},{18,19},{21,21},{23,...},{...}]},
 {"proposition",[{3,3}]},
 {"that",[{3,3},{6,...},{...}]},
 {"all",[{3,...}]},
 {"men",[{...}]},
 {"are",[...]},
 {[...],...},
 {...}|...]
```

#### Data Source 2

```
index:runner().
 [{"A",
  [{3,3},
   {77,77},
   {255,255},
   {259,259},
   {684,684},
   {781,781},
   {870,870},
   {1283,1283},
   {1430,1430},
   {1588,1588},
   {2152,2154},
   {2247,2247},
   {2997,2997},
   {3093,3093},
   {3126,3126},
   {3389,3389},
   {3393,3393},
   {3489,3490},
   {3617,3617},
   {3631,3631},
   {3643,3643},
   {3715,3715}]},
 {"CHRISTMAS",[{3,3}]},
 {"CAROL",[{3,3}]},
 {"by",
  [{5,5},
   {21,21},
   {44,44},
   {196,196},
   {204,204},
   {219,219},
   {289,289},
   {402,402},
   {493,493},
   {521,521},
   {567,567},
   {598,598},
   {690,690},
   {703,703},
   {718,718},
   {758,758},
   {771,771},
   {780,780},
   {821,821},
   {904,904},
   {906,...},
   {...}|...]},
 {"Charles",[{5,5}]},
 {"Dickens",[{5,5}]},
 ...
```


### TODO

* Unit Tests
* Clean up functions in index.erl


### System

* Erlang/OTP 19
* MacOS
