package shapelessTest

object TypeCheckingDemo {
 def main(args: Array[String]): Unit ={
   import shapeless.test.illTyped
   illTyped{"""1+1 : Boolean"""}
   //illTyped{"""1+1 : Int"""}

 }
}
