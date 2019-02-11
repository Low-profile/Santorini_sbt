import spray.json._
import DefaultJsonProtocol._

type Space = List[List[Int]]
type Players = List[List[List[Int]]]

def printList(args: TraversableOnce[_]): Unit = {
  args.foreach(println)
}

val testjson = " {\"turn\":19,\n\n   \"players\":[[[3,5],[2,5]],[[3,4],[4,4]]],\n\n   \"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}\n"

case class Board(turn: Int, players: Players, spaces: Space)

implicit val boardFormat = jsonFormat3(Board)

val res = testjson.parseJson.convertTo[Board]

//List(List(-1, -1), List(-1, 0), List(-1, 1), List(0, -1), List(0, 1), List(1, -1), List(1, 0), List(1, 1))
val directions = List(-1, 0, 1).flatMap(x => List(-1, 0, 1).map(y => List(x, y))).filterNot(_ == List(0, 0))


def availableMove(b: Board): List[List[List[Int]]] = {
  val mytokens = b.players(0)
  val space = b.spaces
  mytokens.map {
    t => {
      val surroundings = directions.map(
        d => t.zip(d).map(
          x => x._1 + x._2))
      //surroundings
      surroundings.filterNot {
        l => {
          val r = l(0) - 1
          val c = l(1) - 1
          lazy val notOnBoard = r < 0 || r >= 5 || c < 0 || c >= 5
          lazy val isTokenPositioned = b.players.exists(_.contains(l))
          lazy val isTowerTooHigh = (space(r)(c) - space(t(0) - 1)(t(1) - 1)) > 1
          lazy val isTowerBuilt = space(r)(c) == 4
          (notOnBoard || isTokenPositioned || isTowerTooHigh || isTowerBuilt)
        }
      }
    }
  }

}

def evaluateBoard(b: Board): Int = {
  //todo add evaluation
  1
}

def updatePlayer(p: Players, c: Int, v: List[Int]): Players = {
  p
    .updated(0, p(0)
      .updated(c, v))
}


def updateSpace(b: Space, r: Int, c: Int, v: Int): Space = {
  b
    .updated(r, b(r)
      .updated(c, v))
}

def evaluateMoves(b: Board, move: List[List[List[Int]]]): List[(Int,Int)] = {
  //Todo: add strategy here
  //number of possible moves + number of winning move
  //val construction = List.fill(move.length)(1)
  val tmp = move.map {
    t => {
      val build_position = t.zipWithIndex.flatMap { case (m, idx) => {
        val newPlayer = updatePlayer(b.players, idx, m)
        val surroundings = directions.map(
          d => m.zip(d).map(
            x => x._1 + x._2))
        surroundings.filterNot {
          l => {
            val r = l(0) - 1
            val c = l(1) - 1
            lazy val notOnBoard = r < 0 || r >= 5 || c < 0 || c >= 5
            lazy val isTokenPositioned = newPlayer.exists(_.contains(l))
            lazy val isTowerBuilt = b.spaces(r)(c) == 4
            (notOnBoard || isTokenPositioned || isTowerBuilt)
          }
        }
      }}
      val newSpace: List[Space] = build_position.map(
        m => {
          val r = m(0) - 1
          var c = m(1) - 1
          updateSpace(b.spaces, r, c, b.spaces(r)(c))
        }
      )
      val idx = newSpace.map(s => {
        val newBoard = Board(b.turn, b.players, s)
        evaluateBoard(newBoard)
      }
      ).zipWithIndex.maxBy(_._1)_.2
    }
  }
  print(tmp)
  tmp
  //    move.flatten.distinct.map {
  //      //l => if (b.spaces(l(0))(l(1)) == 3) l
  //      l => evaluateBoard()
  //    }


  //List(move(0),List(1))
}



val potentialMoves = availableMove(res)
printList(potentialMoves)
val result = evaluateMoves(res, potentialMoves)
printList(result)
//def solve(b: Board, depth : Int, evaluations : List[(List[Int],Int)]) = {
//  if (depth == 0) None
//  else if (depth == 1){
//    val potentialMoves = availableMove(b)
//    val result = evaluateMoves(potentialMoves)
//  }
//  else{
//
//  }
//
//}

