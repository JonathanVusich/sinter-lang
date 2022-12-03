#[repr(u8)]
#[derive(PartialOrd, PartialEq, Eq, Debug)]
pub(crate) enum OpCode {
    ReturnVoid,
    Return,

    Pop,

    AddUnsigned,
    SubtractUnsigned,
    MultiplyUnsigned,
    DivideUnsigned,

    AddSigned,
    SubtractSigned,
    MultiplySigned,
    DivideSigned,

    AddFloat,
    SubtractFloat,
    MultiplyFloat,
    DivideFloat,

    NegateInteger,
    NegateFloat,

    IntegerToFloat,
    FloatToInteger,

    GetConstant,

    SetLocal,

    GetLocal,

    Jump,
    JumpBack,

    Call,
}

impl From<u8> for OpCode {
    #[inline(always)]
    fn from(byte: u8) -> Self {
        let opcode: OpCode;
        unsafe {
            opcode = std::mem::transmute::<u8, OpCode>(byte);
        }
        opcode
    }
}

impl From<OpCode> for u8 {
    #[inline(always)]
    fn from(opcode: OpCode) -> Self {
        opcode as u8
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use libc::chdir;

    #[test]
    fn test_enum() {
        macro_rules! check_transmution {
            ($identifier:ident) => {
                assert_eq!(OpCode::$identifier, OpCode::from(OpCode::$identifier as u8));
            };
        }

        check_transmution!(ReturnVoid);
        check_transmution!(Return);
        check_transmution!(Pop);

        check_transmution!(AddUnsigned);
        check_transmution!(SubtractUnsigned);
        check_transmution!(MultiplyUnsigned);
        check_transmution!(DivideUnsigned);

        check_transmution!(AddSigned);
        check_transmution!(SubtractSigned);
        check_transmution!(MultiplySigned);
        check_transmution!(DivideSigned);

        check_transmution!(AddFloat);
        check_transmution!(SubtractFloat);
        check_transmution!(MultiplyFloat);
        check_transmution!(DivideFloat);

        check_transmution!(NegateInteger);
        check_transmution!(NegateFloat);

        check_transmution!(GetConstant);
        check_transmution!(SetLocal);
        check_transmution!(GetLocal);
        check_transmution!(Jump);
        check_transmution!(JumpBack);
        check_transmution!(Call);
    }
}
