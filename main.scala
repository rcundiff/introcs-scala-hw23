import scala.util.{ Try, Success, Failure }
import scala.io._
import scala.math._
import java.io._

object main {
   
    def main(args: Array[String]) {        
        println("Enter class name:")
        val courseName = readLine()
        val results = readCategoryFile(courseName)
        results match {
          case (n, c, q, w) => {
              
            val studentstuff = readStudentFile(courseName,c,q,w)      
              
          }
        }
    }
    
   def readStudentFile(courseName : String, categoryArray : Array[String], weightStuff : Array[Int], numberofAssignments : Array[Int]) = {       
     val studentFileName = s"students_$courseName.txt"
     val file = Source.fromFile(studentFileName)
     val lines = file.getLines        
     val sumFileName = courseName + "_summary.txt"        
     val newfile = new File(sumFileName)        
     val bw = new BufferedWriter(new FileWriter(newfile))              
     while (lines.hasNext) {
         var thing = lines.next
         var arrayofstuff = thing.split(",")     
         val id = arrayofstuff(0) + courseName + ".data"
         val datalines = Source.fromFile(id).getLines        
         var comparedata = scala.collection.mutable.Map[String,scala.collection.mutable.Map[String,Double]]()    
         var tracking = scala.collection.mutable.Map[String,Int]()
         while (datalines.hasNext) {        
             val da = datalines.next    
             val data = da.split(",")             
             val name = data(0).trim             
             val num = data(1).trim.toDouble             
             val score = data(2).trim.toDouble             
             if (comparedata.contains(name)) { 
                 val x = comparedata.get(name).get                       
                 var tempnum =  x.get("num").get + num
                 var tempscore = (x.get("score").get + score) 
                 var temptrack = tracking.get(name).get + 1 
                 var newmap = scala.collection.mutable.Map("num" -> tempnum,"score" -> tempscore)
                 
                 comparedata.update(name,newmap)  
                 tracking.update(name,temptrack)  
                 
             } else {      
               comparedata += name -> scala.collection.mutable.Map("num" -> num,"score" -> score)
               tracking += name -> 1  
             }
           
         }
         
         var i = 0        
         var finalscore = 0.0         
         var totalweight = 0.0         
         for (catstring <- categoryArray) {
             if (comparedata.contains(catstring)) { 
                val allscore = comparedata.get(catstring).get.get("score").get  
                val track = tracking.get(catstring).get 
                val truescorewithoutweight = allscore/track  
                finalscore = finalscore + (truescorewithoutweight * weightStuff(i))
             }   
             
             totalweight = totalweight + weightStuff(i)
             i = i + 1
             
         }
         val dataperpersonintofile = arrayofstuff(1) + "," + arrayofstuff(2) + " " + (finalscore / totalweight).toDouble + " " + lettergrade((finalscore / totalweight).toDouble)         
         println(dataperpersonintofile)  
         bw.write(dataperpersonintofile + "\n")
     }    
        
     bw.close()
        
  }
   
   def lettergrade(grade: Double): String = {  
       if (grade > 94.0) {
          return "A+"
      } else if (grade > 90.0) {
          return "A"
      } else if (grade > 87.0) {
          return "B+"
      } else if (grade > 84.0) {
          return "B"
      } else if (grade > 80.0) {
          return "B-"
      } else if (grade > 77.0) {
          return "C+"
      } else if (grade > 74.0) {
          return "C"
      } else if (grade > 70.0) {
          return "C-"
      } else if (grade > 67.0) {
          return "D+"
      } else if (grade > 64.0) {
          return "D"
      } else if (grade > 60.0) {
          return "D-"
      } else {
          return "F"
      }
      return "Error"
   } 
    
  def readCategoryFile(courseName : String) : (Int, Array[String], Array[Int], Array[Int]) = {
    val courseFileName = s"categories_$courseName.txt"
    val file = Source.fromFile(courseFileName)
    val lines = file.getLines
    val category = lines.next
    val categoryNames = takecategories(category)
    val quantities = lines.next
    val quantitiesArray = takenumerofassignments(quantities, -1)
    val weights = lines.next
    val weightsArray = takeweights(weights, -1)

    val columns = min(min(categoryNames.length, quantitiesArray.length), weightsArray.length)

    (columns, categoryNames, quantitiesArray, weightsArray)
   }    
    
  def takecategories(line : String) : Array[String] = {
    val tokens = line.split(",")
    for (i <- 0 until tokens.length)
       tokens(i) = tokens(i).trim
    tokens
  }

  def takeweights(line : String, failValue : Int) : Array[Int] = {

    val tokens = line.split(",")
    val integers = Array.fill(tokens.length)(failValue)
    for (i <- 0 until tokens.length) {
      integers(i) = Try(tokens(i).trim.toInt) getOrElse(failValue)
    } 
    integers
  }

  def takenumerofassignments(line : String, failValue : Int) : Array[Int] = {

    val tokens = line.split(",")
    val integers = Array.fill(tokens.length)(failValue)
    for (i <- 0 until tokens.length) {
      integers(i) = Try(tokens(i).trim.toInt) getOrElse(failValue)
    } 
    integers
  }

       
}
    