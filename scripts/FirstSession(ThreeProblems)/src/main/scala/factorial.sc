def factorialTail(x: Integer) = {
  def factorialIteration(product: Integer, counter: Integer): Integer = {
    if(counter > x)
      product
    else
      factorialIteration(product * counter, counter + 1)
  }

  factorialIteration(1, 1)
}

factorialTail(5)




