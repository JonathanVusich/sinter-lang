#[repr(u8)]
#[derive(PartialOrd, PartialEq, Eq, Debug)]
pub (crate) enum OpCode {
    ReturnVoid,
    Return32,
    Return64,

    Pop32,
    Pop64,

    GetConstant32,
    GetConstant64,

    SetLocal32,
    SetLocal64,

    GetLocal32,
    GetLocal64,

    Jump,
    JumpBack,

    Call,
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
        assert_eq!(OpCode::ReturnVoid, OpCode::from(0));
        assert_eq!(OpCode::Return32, OpCode::from(1));
        assert_eq!(OpCode::Return64, OpCode::from(2));
        assert_eq!(OpCode::Pop32, OpCode::from(3));
        assert_eq!(OpCode::Pop64, OpCode::from(4));
        assert_eq!(OpCode::GetConstant32, OpCode::from(5));
        assert_eq!(OpCode::GetConstant64, OpCode::from(6));
        assert_eq!(OpCode::SetLocal32, OpCode::from(7));
        assert_eq!(OpCode::SetLocal64, OpCode::from(8));
        assert_eq!(OpCode::GetLocal32, OpCode::from(9));
        assert_eq!(OpCode::GetLocal64, OpCode::from(10));

        assert_eq!(OpCode::Jump, OpCode::from(11));
        assert_eq!(OpCode::JumpBack, OpCode::from(12));
        assert_eq!(OpCode::Call, OpCode::from(13));



    }
}