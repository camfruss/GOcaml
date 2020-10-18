# Go Games

Here we document what constitues a valid JSON Go game file.

## General Format

There are three required properties in the first level of the JSON file: `players`, `board`, and `config`:
```
{
  "players" : {},
  "board" : {},
  "config" : {}
}
```

## Players

`p#` is the player number. This is either `p1` for player 1 or `p2` for player 2. A valid game must include both `p1` and `p2` keys.
`byoyomi` is the number of byoyomi periods remaining for that player. Once all periods are used, the player loses on time.
`game_time` is the time remaining on that player's main time clock.
`id` is the name of the player.
`prisoners` is a list of all the stones captured, with each integer representing the number of stones on the board after the capture. This helps with detecting ko violations later in the game. 
`stone` is the player's stone color. This can either be `"b"` or `"w`. The two players must have different stone colors.

```
p# : {
  "byoyomi" : int,
  "game_time" : int,
  "id" : string,
  "prisoners" : int list,
  "stone" : string
}
```

## Board

`size` must be an odd integer on the interval `[3, 25)`. While most go boards are either 9x9, 13x13, or 19x19, we are allowing for more customization in this field. 
`white` and `black` are arays consisting of all the moves made by the two players. More details are below.
```
{
  "size" : int,
  "white" : move array,
  "black" : move array
}
```

### Move

A single move is represented by three fields, `col`, `row`, and `cur_stones`. The column and row are the column and row the stone was placed on and the `cur_stones` represents the number of stones on the board after the move is completed. This value allows for an easier detection of ko violations later on in the game. 
```
{
  "col" : int,
  "row" : int,
  "cur_stones" : int
}
```

## Config

`byoyomi_period` is the number of seconds in a single byoyomi period.
`komi` is a positive real number and represents the compensation awarded to the player who went second in the game (customarily white). This is usually a non-integer value to avoid ties.
`turn` denotes the player whose turn it is. This must either be `"w"` or `"b"`, representing White's or Black's turn.
```
{
  "byoyomi_period" : int,
  "komi" : float,
  "turn" : string
}
```
