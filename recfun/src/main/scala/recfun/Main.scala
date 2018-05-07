package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
        if (r < c ) 
          0
        else if (c == 0 || r == c)
          1 
        else 
          pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
        def balanceRec(chars: List[Char], openBraceCount: Int): Boolean = {
          if (chars.isEmpty) openBraceCount == 0
            else
              if (chars.head == '(')  balanceRec(chars.tail, openBraceCount + 1)
                else
                  if (chars.head == ')') openBraceCount>0 && balanceRec(chars.tail, openBraceCount - 1)
                    else balanceRec(chars.tail , openBraceCount)
        }
        balanceRec(chars,0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
        
        //base cases

        //if no money to return
        if(money == 0)
          return 1
        
        //no way if money is negative
        if (money < 0)
          return 0
        
        //if no coins and got money to return
        if(coins.isEmpty && money > 0)
          return 0
        
        //classic dp substructure problem
        //with atleast one coin (head) and without it
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
