#[repr(u8)]
#[derive(PartialOrd, PartialEq, Eq, Debug)]
pub (crate) enum OpCode {
    ReturnVoid,

    Return8,
    Return16,
    Return32,
    Return64,

    Pop8,
    Pop16,
    Pop32,
    Pop64,

    GetConstant8,
    GetConstant16,
    GetConstant32,
    GetConstant64,

    SetLocal8,
    SetLocal16,
    SetLocal32,
    SetLocal64,

    GetLocal8,
    GetLocal16,
    GetLocal32,
    GetLocal64,

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_enum() {
        assert_eq!(OpCode::ReturnVoid, OpCode::from(0));

        assert_eq!(OpCode::Return8, OpCode::from(1));
        assert_eq!(OpCode::Return16, OpCode::from(2));
        assert_eq!(OpCode::Return32, OpCode::from(3));
        assert_eq!(OpCode::Return64, OpCode::from(4));

        assert_eq!(OpCode::Pop8, OpCode::from(5));
        assert_eq!(OpCode::Pop16, OpCode::from(6));
        assert_eq!(OpCode::Pop32, OpCode::from(7));
        assert_eq!(OpCode::Pop64, OpCode::from(8));

        assert_eq!(OpCode::GetConstant8, OpCode::from(9));
        assert_eq!(OpCode::GetConstant16, OpCode::from(10));
        assert_eq!(OpCode::GetConstant32, OpCode::from(11));
        assert_eq!(OpCode::GetConstant64, OpCode::from(12));

        assert_eq!(OpCode::SetLocal8, OpCode::from(13));
        assert_eq!(OpCode::SetLocal16, OpCode::from(14));
        assert_eq!(OpCode::SetLocal32, OpCode::from(15));
        assert_eq!(OpCode::SetLocal64, OpCode::from(16));

        assert_eq!(OpCode::GetLocal8, OpCode::from(17));
        assert_eq!(OpCode::GetLocal16, OpCode::from(18));
        assert_eq!(OpCode::GetLocal32, OpCode::from(19));
        assert_eq!(OpCode::GetLocal64, OpCode::from(20));

        assert_eq!(OpCode::Jump, OpCode::from(21));
        assert_eq!(OpCode::JumpBack, OpCode::from(22));
        assert_eq!(OpCode::Call, OpCode::from(23));
    }
}