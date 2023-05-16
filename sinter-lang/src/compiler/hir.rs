use std::collections::HashSet;
use hashbrown::HashMap;
use crate::compiler::ast::NodeId;

struct ModId {
    id: u32,
}

struct HirId {
    mod_id: ModId,
    node_id: NodeId,
}

struct HirNode {
    
}

struct HirMap {
    main_method: HirId,
    modules: HashSet<ModId>,
    nodes: HashMap<HirId, HirNode>,
}