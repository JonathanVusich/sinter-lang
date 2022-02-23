pub fn disassemble_class(class_reader: impl ByteReader) -> Vec<String> {
    let output = vec![];
    let compiled_class = CompiledClass::load(class_reader);
}

mod tests {

    #[test]
    pub fn dis_class() {
        let class = CompiledClass {

        };

        let byte_writer = ByteWriter::

        let output = disassemble_class(class);
    }
}