package edu.colorado.csci3155.project1

// Jacob Christiansen

object StackMachineCompiler {

    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction]=e match{

        case Const(n) =>{
            List(PushI(n))
        }


        case Plus(e1, e2) =>{
            val x1=compileToStackMachineCode(e1)
            val x2=compileToStackMachineCode(e2)
            val x3= x1++ x2
            return x3++List(AddI)
        }


        case Minus(e1,e2) => {
            val x1=compileToStackMachineCode(e1)
            val x2=compileToStackMachineCode(e2)
            val x3= x1 ++x2
            return x3++List(SubI)
        }


        case Mult(e1,e2) =>{
            val x1=compileToStackMachineCode(e1)
            val x2= compileToStackMachineCode(e2)
            val x3= x1 ++ x2
            return x3++ List(MultI)
        }


        case Div(e1, e2) =>{
            val x1=compileToStackMachineCode(e1)
            val x2= compileToStackMachineCode(e2)
            val x3=x1++x2
            return x3++List(DivI)
        }


        case Log(e) =>{
            val x=compileToStackMachineCode(e)
            return x++ List(LogI)
        }


        case Exp(e) =>{
            val x = compileToStackMachineCode(e)
            return x++ List(ExpI)
        }


        case Sine(e) => {
            val x = compileToStackMachineCode(e)
            return x++List(SinI)
        }


        case Cosine(e) =>{
            val x =compileToStackMachineCode(e)
            return x ++List(CosI)
        }


    }
}