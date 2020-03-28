package gridworld.world

import scala.util.Random
import scala.collection.mutable.ArrayBuffer
//has random access or update with O(1) YEAH!

trait Matrix {
      type M
      
      protected val matrix: ArrayBuffer[ArrayBuffer[M]]
      val dimX: Int
      val dimY: Int

      def dims: (Int, Int) = (dimY, dimX)
      def nrow: Int = dimX
      def ncol: Int = dimY


      def where(what: M): (Int, Int) = {
     	 var result = (-1, -1)
     	 for (y <- 0 to (nrow - 1)){
	     matrix(y).indexOf(what) match {
		 	     case -1      => ()
			     case x: Int  => result = (y,x)
	     }
	 }
	 result
     }
      
      def at(y: Int, x: Int): M = matrix(y)(x)

      def update(y: Int, x: Int)(value: M) = {matrix(y)(x) = value}


      override def toString(): String = {
     	      var string: String = s"\n"
	      val space = if (nrow > 10) {""} else {" "}
	      val ndash = if (nrow > 10) {2} else {4}
	      val sep = "" + "-"*ndash*(nrow-1) + "-"*(ndash-1) + "\n"
	      
     	      for (y <- 0 to (dimY - 1)){
	      	  //since M can be any type we need to convert each
		  //element to string before proceding with the reduce
	      	  var row = matrix(y) map    (_.toString) reduce (_ + space + "|" + space + _)
		  string = if (y != dimY - 1){
		  	      	 string + space + row + space + "\n" + sep
			   }else {
			   	 string + space + row + space + "\n"
			   }
	      }
	      string
		  
     }
}

case class World(dimY: Int, dimX: Int) extends Matrix {
     type M = String
     val dimX_1 = nrow - 1
     val dimY_1 = ncol - 1
     protected val matrix: ArrayBuffer[ArrayBuffer[M]] = ArrayBuffer
     	       	   	   			       	 .fill(dimY, dimX)(" ")
     
     update(dimY/2, dimX/2)("X") //This is the Treasure

     //Need to adjust the walls later
     val walls = Seq[(Int,Int)]((1,1),(4,4),(6,6))

     def wall_adder(walls: Seq[(Int,Int)]): Unit = {
	 for (w <- walls){
	     w match {
	       case (y, x) => update(y,x)("W")
	     }
	 }
     }

     def move(how: String, char: String): Int = {
     	 var (y, x): (Int,Int) = where(char)
     	 val res: Int = how match {
	     case "up"    => y match {
	     	  	       case 0  => -1
			       case -1 => -2
			       case y1 => at(y-1,x) match {
			       	       	  	case " " => matrix(y)(x) = " "
			       	       	  	       	    matrix(y-1)(x) = char
					  		    1
						case "W" => 2
						case "X" => eatAt(y-1,x,char)
						     	    matrix(y)(x) = " "
							    3
						case _   => 0
			       	       	  }
			     }
	     case "down"  => y match {
	     	  	       case `dimY_1`  => -1
			       case -1        => 0
			       case y1 => at(y+1,x) match {
			       	       	  	case " " => matrix(y)(x) = " "
			       	       	  	       	    matrix(y+1)(x) = char
					  		    1
						case "W" => 2
						case "X" => eatAt(y+1,x,char)
						     	    matrix(y)(x) = " "
							    3
						case _   => 0
			       	       	  }
			     }
	     case "right" => x match {
	     	  	       case `dimX_1` => -1
			       case -1 	     => -2
			       case x1 => at(y,x+1) match {
			       	       	  	case " " => matrix(y)(x) = " "
			       	       	  	       	    matrix(y)(x+1) = char
					  		    1
						case "W" => 2
						case "X" => eatAt(y,x+1,char)
						     	    matrix(y)(x) = " "
							    3
						case _   => 0
			       	       	  }
			     }
	     case "left"  => x match {
	     	  	       case 0  => -1
			       case -1 => -2
			       case x1 => at(y,x-1) match {
			       	       	  	case " " => matrix(y)(x) = " "
			       	       	  	       	    matrix(y)(x-1) = char
					  		    1
						case "W" => 2
						case "X" => eatAt(y,x-1,char)
						     	    matrix(y)(x) = " "
							    3
						case _   => 0
			       	       	  }
			     }
	     case _       => -3
	 }
	 res
     }

     def eatAt(y: Int, x: Int, eater: String): Unit = {
     	 assert(at(y,x) != " " && at(y,x) != "W",
	 	s"$eater tried to eat emptiness or a Wall! at ($y, $x)") 
	 update(y,x)(eater)
     }

     def reset(): Unit = {
     	 for (y <- 0 to dimY_1){
	     for (x <- 0 to dimX_1){update(y,x)(" ")}
	 } 
	 update(dimY/2, dimX/2)("X") //This is the Treasure
     }
}

