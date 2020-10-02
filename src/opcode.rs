#[derive(Debug)]
pub(crate) enum OpCode {
    OpReturn,
    OpIntConstant(u16),
    OpLongConstant(u16),
    OpFloatConstant(u16),
    OpDoubleConstant(u16)
}