package edu.colorado.csci3155.project1

// Jacob Christiansen
sealed trait StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {

    def emulateSingleInstruction(stack: List[Double], ins: StackMachineInstruction): List[Double]=ins match{

        case PushI(f)=>f::stack;


        case PopI => stack match{
            case x1::x_bot =>{
                x_bot
            }
            case Nil => throw new IllegalArgumentException()
        }


        case AddI => stack match{
            case x1::x2::x_bot => x2::x_bot match{
                case x2::x_bot=> x1+x2::x_bot

                case Nil => throw new IllegalArgumentException()
            }

            case Nil => throw new IllegalArgumentException()
        }


        case SubI => stack match{
            case x1::x2::x_bot => x2::x_bot match{

                case x2::x_bot => {
                    (x2-x1)::x_bot
                }

                case Nil=> throw new IllegalArgumentException()
            }
            case Nil => throw new IllegalArgumentException()
        }


        case MultI => stack match{

            case x1::x2::x_bot => x2::x_bot match{
                case x2::x_bot => x1*x2::x_bot

                case Nil => throw new IllegalArgumentException()
            }
            case Nil => throw new IllegalArgumentException()
        }


        case DivI => stack match{
            case x1::x2::x_bot => x2 ::x_bot match{
                case x2::x_bot =>{
                    (x2/x1)::x_bot
                }

                case Nil => throw new IllegalArgumentException()
            }
            case Nil => throw new IllegalArgumentException()

        }


        case LogI => stack match{

            case x_top::x_bot => {
                if(x_top>0){
                    scala.math.log(x_top)::x_bot
                }
                else{
                    throw new IllegalArgumentException()
                }
            }
            case Nil =>throw new IllegalArgumentException()
        }


        case ExpI=> stack match{
            case x_top::x_bot =>{

                scala.math.exp(x_top)::x_bot
            }
            case Nil => throw new IllegalArgumentException()
        }


        case SinI => stack match{
            case x_top::x_bot => {
                scala.math.sin(x_top)::x_bot
            }
            case Nil => throw new IllegalArgumentException()
        }


        case CosI => stack match{
            case x_top::x_bot => {
                scala.math.cos(x_top):: x_bot
            }
            case Nil => throw new IllegalArgumentException()
        }


    }

    def emulateStackMachine(instructionList:List[StackMachineInstruction]): Double={

        instructionList.foldLeft(Nil:List[Double]) ((acc:List[Double], SMI:StackMachineInstruction) => {
            emulateSingleInstruction(acc,SMI)
        })(0)
    }


}