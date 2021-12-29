#[repr(u8)]
#[derive(PartialOrd, PartialEq, Eq, Debug)]
pub (crate) enum OpCode {
    Return,
    I64Constant,
    F64Constant,
    I2F,
    F2I,
    IAdd,
    FAdd,
    ISub,
    FSub,
    IMult,
    FMult,
    IDiv,
    FDiv,
    INeg,
    FNeg
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> Self {
        let opcode: OpCode;
        unsafe {
            opcode = std::mem::transmute::<u8, OpCode>(byte);
        }
        opcode
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_enum() {
        assert_eq!(OpCode::Return, OpCode::from(0));
        assert_eq!(OpCode::I64Constant, OpCode::from(1));
        assert_eq!(OpCode::F64Constant, OpCode::from(2));
    }
}