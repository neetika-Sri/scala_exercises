package shapelessTest
import shapeless._
import syntax.std.function._
import ops.function._
class ArityDemo() {
  def applyProduct[P <: Product, F, L <: HList,R](p:P)(f:F)
    (implicit gen: Generic.Aux[P,L], fp: FnToProduct.Aux[F,L=>R]) = f.toProduct(gen.to(p))
}

