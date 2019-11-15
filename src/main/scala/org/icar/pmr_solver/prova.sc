import org.icar.pmr_solver.RawVar

import scala.collection.mutable.Map

var tokens : Map[RawVar,Boolean] = Map.empty[RawVar,Boolean]
tokens += RawVar(0) -> true
tokens += RawVar(1) -> false

var clone_tokens = tokens.clone()
clone_tokens += RawVar(0) -> false

println(tokens)
println(clone_tokens)



