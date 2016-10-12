package src.main.scala

import scala.io.Source
import scala.math._
import java.io._
import scala.collection.mutable.ListBuffer

object Anneal {
	val numrows = 10
	val numcols = 20
	val theatre = Array.ofDim[String](numrows, numcols)
	var seatscores = Array.ofDim[Double](numrows, numcols)
	var doanneal = true
	val spacebenefit : Double = 0.5
	val endbenefit : Double = 0.2


	case class Reservation(name: String,num: Int)

	class Config(var theatre: Array[Array[String]]) {

		val score = this.calculateScore()

		def calculateScore() : Double = {
			var score : Double = 0
			for ((row, rownum) <- theatre.zipWithIndex) {
				var rowscore : Double = 0
				for (colnum <- 0 until row.length) {
						var seatscore : Double = 0
						if (row(colnum) == null) {} 
						else {
							
							seatscore = seatscores(rownum)(colnum)
							if (colnum > 0 && row(colnum - 1) == null) seatscore += spacebenefit
							if (colnum < row.length - 1 && row(colnum + 1) == null) seatscore += spacebenefit
							if (colnum == 0 || colnum == row.length-1) seatscore += endbenefit
						}
						rowscore += seatscore
					}
				score += rowscore * (1 - (rownum+0.0)/numrows)
			}
			score
		}
	}

	def main(args: Array[String]){

		if (args.length < 1)
		return


		val filename = args(0)
		// convert the input file into a list of reservation objects
		val lines = Source.fromFile(filename).getLines.toList
		.filter{_.length > 1}
		.map{ k=>
			val s = k.split(' ')
			Reservation(s(0), s(1).toInt)
		}

		initSeats() // initialize the baseline scores for seats

		// create an initial configuration based on first fit
		val initc = initConfig(lines)
		println()
		printArr(initc)
		var conf = new Config(initc)
		var best = conf;

		val available = totalSeatsAvailable(initc)

		// this makes it so that if we are already tightly packed,
		// we dont try to do simulated annealing
		if (available > numrows + 1) {
			println("\ninitial score = " + conf.score + "\n")
			// perform simulated annealing
			best = anneal(conf, 10000, .000001)
			printArr(best.theatre)
			println("\nfinal score = " + best.calculateScore())
		}

		val outputfile = args(0).substring(0, args(0).lastIndexOf("."))+".output"
		printReservations(outputfile, best)
		println("Reservations printed to " + outputfile)

	}

	def initSeats() {
		val midpoint : Double = (numcols-1)/2.0
		seatscores = seatscores.zipWithIndex.map{ case (row, rownum) =>
			// this makes it so that rows 0 and 1 are bad, row 2 is pretty good
			// then rows 3,4 and 5 are really good, then declines gradually
			var rowmodifier : Double = (logNormal(rownum) + .1)*4
			row.zipWithIndex.map { case (seatval, colnum) =>
				var seatscore :Double = 0.0					
				seatscore += (1.0 + midpoint - math.abs(midpoint - colnum))
				seatscore *= rowmodifier
				seatscore
			}
		}
	}

	def anneal(conf : Config, inittemp : Double, coolingRate : Double) : Config = {

		var temp = inittemp;

		val r = scala.util.Random

		var row1 = 0 // the row with a Reservation
		var row2 = 0 // the row with empty seats
		var col1 = 0 // the column with the Reservation
		var col2 = 0  // the column with an empty seat
		var length = 0 // the length of the reservation we are swapping
		var length2 = 0 // the length of the empty seats
		var swapcol1 = -1 // the start column of the reservation
		var swapcol2 = -1 // the start column of the empty seats
		var startcol = -1 // used to determing the empty seat region
		var endcol = -1 // used to determing the empty seat region
		var currstring = "" //initializer
		var currentscore : Double = conf.score // current score of the configuration
		var newscore : Double = 0 // score of the configuration after a modification
		var toggle = false // used when determining the empty seat region

		val copy = Array.ofDim[String](numrows, numcols)
		var newconf = new Config(copy)

		val copy2 = Array.ofDim[String](numrows, numcols)
		var best = new Config(copy2)


		while (temp > 1){
			
			copyArr(conf, newconf)
					

			do {

				// find a reservation
				do {
					row1 = r.nextInt(numrows)
					col1 = r.nextInt(numcols)
					currstring = newconf.theatre(row1)(col1)
				} while (currstring == null)

				// since the reservations are contiguous and on the same row, the following is valid
				swapcol1 = newconf.theatre(row1).indexOf(currstring)
				length = newconf.theatre(row1).lastIndexOf(currstring) - swapcol1

				// try to find empty seats that can fit the reservation
				row2 = r.nextInt(numrows)
				col2 = r.nextInt(numcols)
				currstring = newconf.theatre(row2)(col2)
				// if the spot we selected is empty...
				if (currstring == null){
					startcol = col2
					endcol = col2
					length2 = endcol - startcol
					// start at the spot that we selected and expand on either side.
					// stop expanding if we can't go any farther on either side
					// OR if we have discovered a large enough region
					while  (length2 < length 
						&& (((startcol > 0) && (newconf.theatre(row2)(startcol-1) == null))
						|| ((endcol < numcols-1) && (newconf.theatre(row2)(endcol+1) == null)))) {
						if (toggle && startcol > 0 && newconf.theatre(row2)(startcol-1) == null)
							startcol -= 1
						else if (!toggle && endcol < numcols-1 && newconf.theatre(row2)(endcol+1) == null)
							endcol += 1

						length2 = endcol - startcol
						toggle = !toggle
					}
					// if we were successful in the previous while loop, then exit the loop
					if (length2 == length) swapcol2 = startcol
				}
			} while (swapcol2 == -1)

			// swap the reservation with the empty seats
			for (i <- 0 to length){
				newconf.theatre(row2)(swapcol2 + i) = newconf.theatre(row1)(swapcol1 + i)
				newconf.theatre(row1)(swapcol1 + i) = null
			}

			swapcol2 = -1 // reset the inner do-while

			// calculate the new score of the new configuration
			newscore = newconf.calculateScore()

			// if we accept the new config
			if (acceptProb(newscore, currentscore, temp) > r.nextDouble()){

				//update the current config
                copyArr(newconf, conf)
                currentscore = newscore
                // update the best config if this is better
                if (newscore > best.calculateScore()){
                	copyArr(newconf, best)
				}
			}

			// cool off some
			temp = temp * (1-coolingRate)
		}

		best
	}

	def acceptProb(e1 : Double, e2 : Double, temp : Double) : Double = {
		if (e2 < e1) {
			return 1.0;
		}
		return math.exp((e1 - e2) / temp);
	}

	//packs in the reservations in a first fit fashion from the front
	def initConfig(list : List[Reservation]) : Array[Array[String]] = {
		val init = theatre.clone 
		for (l <- list){
			var firstfit = init.map{ k=>
				val r = spotsRemaining(k)
				if (r>= l.num) true
				else false
			}.indexOf(true)
			// if the reservation is less than the number of seats in a row
			// but there is no row with that many seats left
			// then stop
			if (l.num > numcols || firstfit == -1) {}
			else {
				var startindex = init(firstfit).indexOf(null)
				for (i <- startindex until startindex + l.num) {
					init(firstfit)(i) = l.name
				}
			}
		}
		return init
	}

	def totalSeatsAvailable(a :Array[Array[String]]) : Int = {
		var count = 0
		for (row <- a)
			for (col <- row)
				if (col != null)
					count += 1

		count
	}

	//the number of spots left in a row
	def spotsRemaining(row : Array[String]) : Int = {
		if (row.indexOf(null) == -1)
			return 0;
		row.length - row.indexOf(null)
	}

	def copyArr(from : Config, to : Config) {
		val height = from.theatre.length
		val width = from.theatre(1).length
		for (i <- 0 until height)
				for (j <- 0 until width) 
					to.theatre(i)(j) = from.theatre(i)(j)
	}

	def printArr[T: ClassManifest](a : Array[Array[T]]) {
		for (row <- a){
			for (col <- row)
				if (col == null)
					print("|    |")
				else 
					print("|"+col+"|")
			println()
		}
	}

	def seatString(row : Int, col : Int) : String = {
		val realcol = col+1
		Character.toString((row + 65).toChar) + realcol
	}

	def printReservations(outputfile : String, conf : Config) {
		var assigns = scala.collection.immutable.TreeMap[String, ListBuffer[String]]()
		val height = conf.theatre.length
		val width = conf.theatre(1).length
		for (i <- 0 until height) {
			for (j <- 0 until width) {
				var rstring = conf.theatre(i)(j)
				if (rstring != null){
					var rlist = assigns.getOrElse(rstring, new ListBuffer[String]())
					rlist += seatString(i, j)
					assigns = assigns + (rstring -> rlist)
				}
			}
		}

		val file = new File(outputfile)
		val bw = new BufferedWriter(new FileWriter(file))

		for ((rstring, rlist) <- assigns) {
			bw.write(rstring+" " + rlist.mkString(",")+"\n")
		}
		bw.close()

	}

	def logNormal(x : Int) : Double = {
		val mu = 1.5
		val sigma = .5
		(exp(-(pow((log(x+0.1)-mu),2))/(2*sigma*sigma)))/(sigma*(x+0.1)*sqrt(2*Pi))
	}


}
