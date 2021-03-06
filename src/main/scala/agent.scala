package gridworld.agent

import gridworld.world._
import gridworld.qmatrix._
import scala.util.Random

trait Q_Agent {
     type Move
     //Which type the moves should be (Int, String, Strange Objects, functions)

     val QVals: QValHolder

     def move(which: Move): Double //should return the reward
     def reset(): Unit //Just a way to reset to a initial state

     def QAlgo(epochs: Int, maxSteps: Int, epsilon: Double, alpha:Double, gamma: Double): Unit
     /* This should implement the chosen QAlgorithm
     It could be implemented using abstact functions and stuff but
     it's left to be implemented by the user to make him understand better
     how it works, the parameters are the following:
     
     episodes = number of games
     maxSteps = max number of steps per game
     epsilon  = probability og a random move between 0 and 1
     alpha    = learning rate
     gamma    = discount factor

     The Q formula is:
     Q(state,move) = Q(state,move) + alpha*(reward + gamma*Q(nextState,nextMove) - Q(state,move))
     where the nextState is the state after the move has taken place and the NextMove depends
     on how you decide to choose it.
     */ 
}

case class Agent(char :String, dim: Int) extends Q_Agent{
     type Move = Int
     
     val dim_1 = dim - 1
     val world = World(dim, dim)
     val QVals = QMap(dim,dim,4)
     world.randomUpdate(char)

     val possibleMoves = Map(0 -> "up",   1 -> "down",
     	       	       	     2 -> "left", 3 -> "right")

     val inverseMoves = Map("up"   -> 0, "down"  -> 1,
     	 	      	    "left" -> 2, "right" -> 3)

     def move(which: Move): Double = {
     	 val how = possibleMoves(which)
     	 val event = world.move(how, char)

	 val score = event match {
	       case -3 => //Out of cases what!?
	       	       	  println("Well it's an invalid move!")
			  -999
	       case -2 => //My coordinates are -1,-1 WTH!?
	       	       	  println("Player not on the map!")
			  -999
	       case -1 => //Ahia border
	       	       	  -1
	       case  0 => //Strange neither wall nor treasure but not empty
	       	       	  println("A non invited character is there! Eliminated!")
			  println(world.toString)
			  -0.1
	       case  1 => //Meh Me moving
	       	       	  -0.1
	       case  2 => //Ahia Wall
	       	       	  -0.5
	       case  3 => //Got the Treasure
	       	       	  5
	       case  _ => //Impossible!? may be you no implement the case yet!
	       	       	  println("What happened case in GridWorld but not in Agent!")
			  -999
	 }		  		
	 assert(score != -999,
	 s"""Go recheck the implementation you Idiot! 
	 \n Plus you should have received a message above indicating the case""")
	 score
	 
     }

     def reset(): Unit = {
     	 world.reset()
	 world.randomUpdate(char)
     }

     def QAlgo(episodes: Int, maxSteps: Int, epsilon: Double, alpha: Double, gamma: Double): Unit = {
     	 for (ep <- 1 to episodes){
	     reset()
	     var tot_reward = 0.0
	     var steps = 0
	     var now_score = 0.0
	     do{
		 steps += 1
	     	 val (y,x) = world.where(char)
	     	 val now_move = if (Random.nextDouble < epsilon) {
		    Random.nextInt(possibleMoves.size)
		 } else {
		    QVals.bestMoveOn(y,x)
		 }
		 val oldQScore = QVals.getQval(y,x,now_move)
		 
		 now_score = move(now_move)
		 
		 val (yNew,xNew) = world.where(char)
		 val bestQScore = QVals.getBestQval(yNew,xNew)
		 
		 val newQScore = oldQScore + alpha*(now_score + gamma*bestQScore - oldQScore) 

		 QVals.updateQval(y,x,now_move)(newQScore)
		 
		 tot_reward = tot_reward + now_score
		 if (now_score == 5) {
		    reset()
		 }
	     } while (steps < maxSteps & now_score != 5)
	     if (ep % 100 == 0)
	     	println(s"Score in $ep is $tot_reward with $steps steps.")
	 }
     }
     
     override def toString(): String = {
     	      "I am '" + char + "' and here is my world: \n" + world.toString
     }

     def showAGame(maxSteps: Int): Unit = {
     	 reset()
	 var steps = 0
	 var score = 0.0
     	 do{
	     val (y,x) = world.where(char)
	     steps += 1
	     val bestMove = QVals.bestMoveOn(y,x)
	     score = move(bestMove)
	     println(world)
	     Thread.sleep(500)
	 } while(steps < maxSteps & score!=5)
	 reset()
     }
}