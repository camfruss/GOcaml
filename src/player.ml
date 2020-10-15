
type stones = Black | White

type t = {
  id : string;
  prisoners : int list;
  stone : stones;
  byoyomi : int;
  game_time : int;
}
