package gridworld.qmatrix

abstract class QValHolder{
/*
This abstract class defines the basic structure that any Q value holder you
implement should have to work: i.e. functions such as getQval and updateQval

An example is defined with the QMap class which uses a Map structure to keep
the Q-values with states defined by a Tuple of String and Int.
For details see the QMap class description.
*/
	 type T
	 type State
	 type Move
	 protected val QMatrix: T
	 /*
	  any datastructure that holds the Qvals
	  it could be also be a Neural Network... and it shouldn't be
	  accessed from outside!
	 */

	 /*
	 you can override the following as public if you want to!
	 It's recommended to create a companion function with easily
	 understandable parameters see QMap example because sometimes
	 the states representation can be intimidating
	 For example in a complex driving system a state can be represented
	 with a lot of positional indicator it is easier to let user say
	 getBestQval(object) and implement it's conversion to getBestQval(state)
	 inside the latter.
	 */
	 protected def getQval(state: State, move: Move): Double
	 protected def updateQval(state: State, move: Move)(Qval: Double): Unit
	 protected def bestMoveOn(state: State): Move
	 protected def getBestQval(state: State): Double
}

case class QMap(dimY: Int, dimX: Int, nmoves: Int) extends QValHolder {
/*
Example implementation of the QMatrix with mutable Map structure,
the State is defined by a String given by a string concatenation of
the row and column coordinate of the grid world and an Integer representing
a possible move.
*/
     type T = scala.collection.mutable.Map[String, Seq[Double]]
     type State = String
     type Move = Int

     def productConcat(x1: Seq[_],x2: Seq[_]): Seq[String] = {
     	 (x1 map {case x => x2 map (x.toString + _.toString)}).flatten
     }

     protected val states = (productConcat(0 to (dimY-1), 0 to (dimX-1)))
     	       	   	    .toList.sorted
     protected val QMatrix = 
     	 scala.collection.mutable.Map(states map (_ -> Seq.fill(nmoves)(0.0)): _*)


     // Two operators to facilitate the use in a transparent way, since a user
     // is not always supposed to know how you implemented your State type.
     protected def getQval(state: State, move: Move): Double = {
     	 QMatrix(state)(move)
     }
     def getQval(y: Int, x: Int, move: Move): Double = {
     	      getQval(y.toString + x.toString, move)
     }


     protected def updateQval(state: State, move: Move)(Qvalue: Double): Unit = {
     	 val movesQ = QMatrix(state).updated(move, Qvalue)
     	 QMatrix(state) = movesQ
     }
     def updateQval(y: Int, x: Int, move: Int)(Qvalue: Double): Unit = {
     	 updateQval(y.toString + x.toString, move)(Qvalue)
     }

     protected def bestMoveOn(state: State): Move = {
     	 val Qvals = QMatrix(state)
     	 Qvals.indexOf(Qvals.max)
     }
     def bestMoveOn(y: Int, x: Int): Move = {
     	 bestMoveOn(y.toString + x.toString)
     }

     protected def getBestQval(state: State): Double = {
     	 QMatrix(state).max
     }
     def getBestQval(y: Int, x: Int): Double = {
     	 getBestQval(y.toString + x.toString)
     }
     
     override def toString(): String = {
          var string = s""
    	  for {x <- states}{
      	      string = string + x + "->" + QMatrix(x).toString + "\n"
	  }
	  string
	      
     }
}

