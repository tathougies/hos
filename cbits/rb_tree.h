#ifndef __rb_tree_H__
#define __rb_tree_H__

#define RB_TREE_ENTRY(for_type) struct {		\
    struct for_type *rb_left, *rb_right;		\
  }

#define RB_TREE_NODE_INITIALIZER {NULL, NULL}

#define DECL_RB_TREE(for_type, key_type)			    \
  for_type *RB_TREE_LOOKUP(for_type, for_type *tree, key_type key); \
  void RB_TREE_INSERT(for_type, for_type **tree, for_type *node);   \
  for_type *RB_TREE_DELETE(for_type, for_type **tree, key_type key); \
  for_type * RB_DELETE_MAX(for_type, for_type **root, for_type **parent);

#define RB_TREE_IS_BLACK(node) ((((uintptr_t) (node)) & 0x1) == 0x0)
#define RB_TREE_IS_RED(node) ((((uintptr_t) (node)) & 0x1) == 0x1)
#define RB_TREE_RED(node) (node ? ((typeof(node)) (((uintptr_t) (node)) | 0x1)) : node)
#define RB_TREE_BLACK(node) ((typeof(node)) (((uintptr_t) (node)) & ~0x1))
#define RB_TREE_PTR(node) ((typeof(node)) (((uintptr_t) (node)) & ~0x1))
#define RB_TREE_LEFT(node, link) (RB_TREE_PTR(node)->link.rb_left)
#define RB_TREE_RIGHT(node, link) (RB_TREE_PTR(node)->link.rb_right)
#define RB_TREE_KEY(node, key_field) (RB_TREE_PTR(node)->key_field)
#define RB_TREE_CHILDREN_EQUAL(a, b) (RB_TREE_PTR(a) == RB_TREE_PTR(b))

#define RB_TREE_MATCH_COLOR(to_match, x) (RB_TREE_IS_BLACK(to_match) ? RB_TREE_BLACK(x) : RB_TREE_RED(x))

/*
 * Insertion
 * --------
 *
 *  four different red violations
 *
 *   grandparent      /     \                     /            \
 *  *grandparent   C(b)     A(b)               A(b)            A(b)
 *                  /         \                 /                \
 *   parent        /           \               /                  \
 *  *parent     B(r)          B(r)           B(r)                B(r)
 *               /               \             \                  /
 *   cur        /                 \             \                /
 *  *cur      A(r)                C(r)         C(r)           C(r)
 *
 *            ||                   ||          ||              ||
 *            \/                   \/          \/              \/
 *
 *           B(b)                 B(b)         C(b)            C(b)
 *           / \                  / \          / \             / \
 *          /   \                /   \        /   \           /   \
 *        A(r)  C(r)           A(r)  C(r)    A(r) B(r)       A(r) B(r)
 *      (Rotate right)       (Rotate left) (rot left @ b,  (rot right @ b,
 *                                          => right @ c)   => left @ c)
 *
 * conditions:
 *  1) RB_TREE_CHILDREN_EQUAL(RB_TREE_LEFT(*grandparent), *parent) && RB_TREE_CHILDREN_EQUAL(RB_TREE_LEFT(*parent), *cur)
 *  2) RB_TREE_CHILDREN_EQUAL(RB_TREE_RIGHT(*grandparent), *parent) && RB_TREE_CHILDREN_EQUAL(RB_TREE_RIGHT(*parent), *cur)
 *  3) RB_TREE_CHILDREN_EQUAL(RB_TREE_LEFT(*grandparent), *parent) && RB_TREE_CHILDREN_EQUAL(RB_TREE_RIGHT(*parent), *cur)
 *  4) RB_TREE_CHILDREN_EQUAL(RB_TREE_RIGHT(*grandparent), *parent) && RB_TREE_CHILDREN_EQUAL(RB_TREE_LEFT(*parent), *cur)
 *
 * Deletion
 * --------
 * (^ indicates next, cur is @)
 * Case 1:
 *       @B(b)         A(b)
 *       / \   ->     / \
 *      /   \        /	 \
 *    A(r) ^C(b)   1(b) B(r)
 *    / \               / \
 * 1(b) 2(b)         2(b) C(b)
 *
 * Case 2:
 *          GP(r)           GP(b)
 *          /  \            /    \
 *         /    \          /      \
 *       @B(b)  1(*) ->   @B(r)    1r
 *       / \    / \       / \      / \
 *      /   \  2b 3b    A(b) ^Cb  2b 3b
 *    A(b) ^C(b)
 *
 * Case 3:
 * We're the left child of our parent, and our sibling's right child is red.
 * Fix: Rotate left around the sibling ... 
 *          GP(r)              1r
 *          /  \              /  \
 *         /    \            /    \
 *       @B(b)  1b  ->     GP(b)  3b
 *       / \    / \        / \
 *      /   \  2b 3r     @Br 2b
 *    A(b) ^C(b)         / \
 *                      Ab ^Cb
 *
 * Case 4:
 * We're the right child of our parent and our sibling's left child is red
 * Fix: rotate right around sibling
 *          GP(r)              1r
 *          /  \              /  \
 *         /    \            /    \
 *       1b     @Bb  ->     2b    GP(b)
 *       / \    / \                / \
 *      /   \  Ab ^Cb             3b @Br
 *    2r    3b                       / \
 *                                  Ab ^Cb
 *
 * Case 5:
 * We're the left child of our parent, and our sibling's left child is red.
 * Fix: Rotate right around the sibling's left child, and then rotate left around the sibling and the parent
 *          GP(r)            GP(r)              2r
 *          /  \              /  \              / \
 *         /    \            /    \            /   \
 *       @B(b)  1b  ->     @B(b)  2r ->    GP(b)    1b
 *       / \    / \        / \     \        /        \
 *      /   \  2r 3b     Ab  ^Cb    1b   @Br         3b
 *    A(b) ^C(b)                     \   / \
 *                                   3b Ab ^Cb
 *
 * Case 6:
 * We're the right child of our parent and our sibling's right child is red.
 */
#define RB_BR(x) (RB_TREE_IS_BLACK(x) ? "black" : "red")
#define VA_ARGS(...) , ## __VA_ARGS__
// #define RB_LOG(x, ...) printf(x VA_ARGS(__VA_ARGS__))
#define RB_LOG(x, ...) (void)0
#define RB_ASSERT(x) do { if (!(x)) { klog("rb assert" #x "failed: " __FILE__ ":"); klog_hex(__LINE__); assert(0);} } while (0)
#define RB_PUSH_RED_DOWN(for_type, link, parent, cur, next, next_sibling, next_dir) \
  if ( RB_TREE_IS_BLACK(*cur) ) {					\
    /* We only need to change colors if cur is actually black */	\
    RB_LOG("push down: cur(%s) next(%s) next_sibling(%s) %p\n", RB_BR(cur), RB_BR(next), RB_BR(next_sibling), parent); \
    if ( RB_TREE_IS_BLACK(*next) && RB_TREE_IS_RED(*next_sibling) ) {	\
      /* Case 1 */							\
      RB_LOG("Case 1\n");						\
      *cur = RB_TREE_RED(*cur);						\
      if ( next_dir == 1) {						\
	RB_LOG("Case 1.1\n");						\
	*next_sibling = RB_TREE_BLACK(*next_sibling);			\
	RB_ROTATE_RIGHT(next_sibling, cur, link);			\
	/*parent = cur;	*/						\
	cur = &RB_TREE_RIGHT(*cur, link);				\
      } else {								\
	RB_LOG("Case 1.2\n");						\
	*next_sibling = RB_TREE_BLACK(*next_sibling);			\
	RB_ROTATE_LEFT(next_sibling, cur, link);			\
	/*parent = cur;	*/						\
	cur = &RB_TREE_LEFT(*cur, link);				\
      }									\
    } else if ( RB_TREE_IS_BLACK(*next) && RB_TREE_IS_BLACK(*next_sibling) ) { \
      if ( parent ) {							\
	for_type **cur_sibling = (*cur == RB_TREE_LEFT(*parent, link)) ? &RB_TREE_RIGHT(*parent, link) : &RB_TREE_LEFT(*parent, link); \
	RB_LOG("cur sibling %p %p\n", cur_sibling, *cur_sibling);	\
	if ( RB_TREE_IS_BLACK(RB_TREE_LEFT(*cur_sibling, link)) && RB_TREE_IS_BLACK(RB_TREE_RIGHT(*cur_sibling, link)) ) { \
	  /* Case 2 */							\
	  RB_LOG("Case 2\n");						\
	  *parent = RB_TREE_BLACK(*parent);				\
	  *cur = RB_TREE_RED(*cur);					\
	  *cur_sibling = RB_TREE_RED(*cur_sibling);			\
	} else if ( *cur == RB_TREE_LEFT(*parent, link) && RB_TREE_IS_RED(RB_TREE_RIGHT(*cur_sibling, link)) ) { \
	  /* Case 3 */							\
	  RB_LOG("Case 3\n");						\
	  *parent = RB_TREE_BLACK(*parent);				\
	  *cur = RB_TREE_RED(*cur);					\
	  *cur_sibling = RB_TREE_RED(*cur_sibling);			\
	  RB_TREE_RIGHT(*cur_sibling, link) = RB_TREE_BLACK(RB_TREE_RIGHT(*cur_sibling, link)); \
	  RB_ROTATE_LEFT(cur_sibling, parent, link);			\
	  parent = &RB_TREE_LEFT(*parent, link);			\
	} else if ( *cur == RB_TREE_RIGHT(*parent, link) && RB_TREE_IS_RED(RB_TREE_LEFT(*cur_sibling, link)) ) { \
	  /* Case 4 */							\
	  RB_LOG("Case 4\n");						\
	  *parent = RB_TREE_BLACK(*parent);				\
	  *cur = RB_TREE_RED(*cur);					\
	  *cur_sibling = RB_TREE_RED(*cur_sibling);			\
	  RB_TREE_LEFT(*cur_sibling, link) = RB_TREE_BLACK(RB_TREE_LEFT(*cur_sibling, link)); \
	  RB_ROTATE_RIGHT(cur_sibling, parent, link);			\
	  parent = &RB_TREE_RIGHT(*parent, link);			\
	} else if ( *cur == RB_TREE_LEFT(*parent, link) && RB_TREE_IS_RED(RB_TREE_LEFT(*cur_sibling, link)) ) { \
	  /* Case 5 */							\
	  RB_LOG("Case 5\n");						\
	  for_type **sibling_left_child = &RB_TREE_LEFT(*cur_sibling, link); \
	  *parent = RB_TREE_BLACK(*parent);				\
	  *cur = RB_TREE_RED(*cur);					\
	  RB_ROTATE_RIGHT(sibling_left_child, cur_sibling, link);	\
	  RB_ROTATE_LEFT(cur_sibling, parent, link);			\
	  parent = &RB_TREE_LEFT(*parent, link);			\
	} else if ( *cur == RB_TREE_RIGHT(*parent, link) && RB_TREE_IS_RED(RB_TREE_RIGHT(*cur_sibling, link)) ) { \
	  /* Case 6 */							\
	  RB_LOG("Case 6\n");						\
	  for_type **sibling_right_child = &RB_TREE_RIGHT(*cur_sibling, link); \
	  *parent = RB_TREE_BLACK(*parent);				\
	  *cur = RB_TREE_RED(*cur);					\
	  RB_ROTATE_LEFT(sibling_right_child, cur_sibling, link);	\
	  RB_ROTATE_RIGHT(cur_sibling, parent, link);			\
	  parent = &RB_TREE_RIGHT(*parent, link);			\
	} else {							\
	  RB_LOG("Unknown case %d %d %d\n", *cur == RB_TREE_LEFT(*parent, link), RB_TREE_IS_BLACK(RB_TREE_LEFT(*cur_sibling, link)), RB_TREE_IS_BLACK(RB_TREE_RIGHT(*cur_sibling, link))); \
	}								\
      }									\
    }									\
  }

#define RB_ROTATE_RIGHT(pivot_ptr, parent_ptr, link) do {		\
    typeof(*pivot_ptr) new_parent_left = RB_TREE_RIGHT(*pivot_ptr, link), \
      new_parent = *pivot_ptr;						\
    RB_TREE_RIGHT(*pivot_ptr, link) = *parent_ptr;			\
    RB_TREE_LEFT(*parent_ptr, link) = new_parent_left;			\
    *parent_ptr = new_parent;						\
    RB_ASSERT(RB_TREE_LEFT(*parent_ptr, link) != *parent_ptr);		\
  } while (0)
#define RB_ROTATE_LEFT(pivot_ptr, parent_ptr, link) do {		\
    typeof(*pivot_ptr) new_parent_right = RB_TREE_LEFT(*pivot_ptr, link), \
      new_parent = *pivot_ptr;						\
    RB_TREE_LEFT(*pivot_ptr, link) = *parent_ptr;			\
    RB_TREE_RIGHT(*parent_ptr, link) = new_parent_right;		\
    *parent_ptr = new_parent;						\
    RB_ASSERT(RB_TREE_LEFT(*parent_ptr, link) != *parent_ptr);		\
  } while (0)

#define RB_TREE_IMPL(for_type, key_type, compare_keys, key_field, link)	\
  for_type *RB_TREE_LOOKUP(for_type, for_type *tree, key_type key) {	\
    if ( !tree ) {							\
      return NULL;							\
    } else {								\
      int cmp = compare_keys(key, RB_TREE_KEY(tree, key_field));	\
      if (cmp == 0)							\
        return RB_TREE_PTR(tree);					\
      else if (cmp == 1)						\
	return RB_TREE_LOOKUP(for_type, RB_TREE_RIGHT(tree, link), key); \
      else								\
	return RB_TREE_LOOKUP(for_type, RB_TREE_LEFT(tree, link), key); \
    }									\
  }									\
									\
  void RB_TREE_INSERT(for_type, for_type **root, for_type *node) {	\
    for_type **grandparent = NULL,					\
      **parent = NULL,							\
      **cur = root;							\
    for (;;) {								\
      int next_dir;							\
      if (!*cur) {							\
	*cur = RB_TREE_RED(node);					\
      }	else if (RB_TREE_IS_RED(RB_TREE_LEFT(*cur, link)) && RB_TREE_IS_RED(RB_TREE_RIGHT(*cur, link))) { \
	/* This node is black and it's full, so we're going to break it apart, and merge it into its parent */ \
	RB_TREE_LEFT(*cur, link) = RB_TREE_BLACK(RB_TREE_LEFT(*cur, link)); \
	RB_TREE_RIGHT(*cur, link) = RB_TREE_BLACK(RB_TREE_RIGHT(*cur, link)); \
	*cur = RB_TREE_RED(*cur);					\
      }									\
      if (parent && RB_TREE_IS_RED(*cur) && RB_TREE_IS_RED(*parent)) {	\
	if ( RB_TREE_CHILDREN_EQUAL(RB_TREE_LEFT(*grandparent, link), *parent) \
	     && RB_TREE_CHILDREN_EQUAL(RB_TREE_LEFT(*parent, link), *cur) ) { \
	  /* Case 1 (see above) */					\
	  *parent = RB_TREE_BLACK(*parent);				\
	  *grandparent = RB_TREE_RED(*grandparent);			\
	  RB_ROTATE_RIGHT(parent, grandparent, link);			\
	  parent = grandparent;						\
	} else if ( RB_TREE_CHILDREN_EQUAL(RB_TREE_RIGHT(*grandparent, link), *parent) && \
		    RB_TREE_CHILDREN_EQUAL(RB_TREE_RIGHT(*parent, link), *cur) ) { \
	  /* Case 2 (see above) */					\
	  *parent = RB_TREE_BLACK(*parent);				\
	  *grandparent = RB_TREE_RED(*grandparent);			\
	  RB_ROTATE_LEFT(parent, grandparent, link);			\
	  parent = grandparent;						\
	} else if ( RB_TREE_CHILDREN_EQUAL(RB_TREE_LEFT(*grandparent, link), *parent) && \
		    RB_TREE_CHILDREN_EQUAL(RB_TREE_RIGHT(*parent, link), *cur) ) { \
	  /* Case 3 (see above) */					\
	  *cur = RB_TREE_BLACK(*cur);					\
	  *grandparent = RB_TREE_RED(*grandparent);			\
	  RB_ROTATE_LEFT(cur, parent, link);				\
	  cur = parent;							\
	  parent = grandparent;						\
	  RB_ROTATE_RIGHT(cur, parent, link);				\
	  cur = parent;							\
	} else {							\
	  /* Case 4 (see above) */					\
	  *cur = RB_TREE_BLACK(*cur);					\
	  *grandparent = RB_TREE_RED(*grandparent);			\
	  RB_ROTATE_RIGHT(cur, parent, link);				\
	  cur = parent;							\
	  parent = grandparent;						\
	  RB_ROTATE_LEFT(cur, parent, link);				\
	  cur = parent;							\
	}								\
      }									\
									\
      next_dir = compare_keys(RB_TREE_KEY(node, key_field), RB_TREE_KEY(*cur, key_field)); \
      if ( next_dir == 0 )						\
	break;								\
									\
      grandparent = parent;						\
      parent = cur;							\
      cur = (next_dir == -1) ? &RB_TREE_LEFT(*cur, link) : &RB_TREE_RIGHT(*cur, link); \
    }									\
    *root = RB_TREE_BLACK(*root);					\
  }									\
									\
  for_type *RB_TREE_DELETE(for_type, for_type **root, key_type key) {	\
    for_type **parent = NULL,						\
      **cur = root,							\
      *ret = NULL;							\
    RB_LOG("Deleting %d\n-------\n", key);				\
    while ( *cur ) {							\
      int next_dir = compare_keys(key, RB_TREE_KEY(*cur, key_field));	\
      for_type **next = (next_dir == 1) ? &RB_TREE_RIGHT(*cur, link) : &RB_TREE_LEFT(*cur, link), \
	**next_sibling = (next_dir == 1) ? &RB_TREE_LEFT(*cur, link) : &RB_TREE_RIGHT(*cur, link); \
      RB_LOG("Going to look at %p %p >>%d<< %d %d %d\n", cur, *cur, RB_TREE_KEY(*cur, key_field), RB_TREE_IS_BLACK(*cur), RB_TREE_IS_BLACK(*next), RB_TREE_IS_BLACK(*next_sibling)); \
      RB_LOG("Next is %p %p\n", *next, *next_sibling);			\
      /* We're going to color cur red consistently */			\
      RB_PUSH_RED_DOWN(for_type, link, parent, cur, next, next_sibling, next_dir); \
      RB_LOG("done adj %d >>%d<<\n", next_dir, RB_TREE_KEY(*cur, key_field)); \
      if ( next_dir == 0 && !ret) {					\
	RB_LOG("Found %p %p %d\n", cur, *cur, RB_TREE_KEY(*cur, key_field));  \
	RB_LOG("CHildren are %p %p\n", RB_TREE_LEFT(*cur, link), RB_TREE_RIGHT(*cur, link));\
	ret = *cur;							\
	RB_LOG("returned key will be %d %p\n", RB_TREE_KEY(ret, key_field), ret); \
	/* We've been found! If we have no children, then just set ourselves to NULL */ \
	/* If we have only one child, then just set ourselves to that child */ \
	/* If we have two children, then replace with the max predecessor */ \
									\
	if ( !RB_TREE_LEFT(*cur, link) && !RB_TREE_RIGHT(*cur, link) ) { \
	  *cur = NULL;							\
	  break;							\
	} else if ( !RB_TREE_LEFT(*cur, link) && RB_TREE_RIGHT(*cur, link) ) { \
	  *cur = RB_TREE_BLACK(RB_TREE_RIGHT(*cur, link));		\
	  break;							\
	} else if ( RB_TREE_LEFT(*cur, link) && !RB_TREE_RIGHT(*cur, link) ) { \
	  *cur = RB_TREE_BLACK(RB_TREE_LEFT(*cur, link));		\
	  break;							\
	} else {							\
	  for_type **old_cur, *succ_node;				\
	  {								\
	    for_type **left_child = &RB_TREE_LEFT(*cur, link),		\
	      **left_right_child = &RB_TREE_RIGHT(*left_child, link),	\
	      **left_left_child = &RB_TREE_LEFT(*left_child, link);	\
	    RB_LOG("Push left %d %p\n", RB_TREE_KEY(*left_child, key_field), parent); \
	    parent = cur;						\
	    cur = left_child;						\
	    RB_LOG("Pushing (parent=%p) (cur = %p) (next=%p) (nxt_sib=%p)\n", *parent, *cur, *left_right_child, *left_left_child); \
	    RB_PUSH_RED_DOWN(for_type, link, parent, cur, left_right_child, left_left_child, 1); \
	    old_cur = parent;						\
	    RB_LOG("After Pushing (parent=%p) (cur = %p) \n", *parent, *cur); \
	    RB_LOG("Set old cur to %p. Ret is %p\n", *old_cur, ret);	\
	    assert(RB_TREE_PTR(*old_cur) == RB_TREE_PTR(ret));		\
	    RB_LOG("Now find max on left %p\n", RB_TREE_RIGHT(*cur, link)); \
	    if ( RB_TREE_RIGHT(*cur, link) ) {				\
	      parent = cur;						\
	      do {							\
		parent = cur;						\
		cur = &RB_TREE_RIGHT(*cur, link);			\
		RB_LOG("Pushing %d right\n", RB_TREE_KEY(*cur, key_field)); \
		for_type **right_child = &RB_TREE_RIGHT(*cur, link);	\
		left_child = &RB_TREE_LEFT(*cur, link);			\
		RB_LOG("Pushing (parent=%p) (cur = %p) (next=%p) (nxt_sib=%p)\n", *parent, *cur, *left_child, *right_child); \
		RB_PUSH_RED_DOWN(for_type, link, parent, cur, right_child, left_child, 1); \
		RB_LOG("After Pushing (parent=%p) (cur = %p) \n", *parent, *cur); \
	      }	while( RB_TREE_RIGHT(*cur, link) );			\
	    }								\
	    RB_LOG("done find max %p %p %p %p\n", cur, *cur, old_cur, *old_cur); \
	    succ_node = *cur;						\
	    *cur = RB_TREE_MATCH_COLOR(succ_node, RB_TREE_LEFT(*cur, link)); \
	    RB_LOG("done deleting max %p\n", succ_node);		\
	    RB_LOG("will assign left %p\n", RB_TREE_PTR(*old_cur));	\
	    RB_TREE_LEFT(succ_node, link) = RB_TREE_LEFT(*old_cur, link); \
	    RB_TREE_RIGHT(succ_node, link) = RB_TREE_RIGHT(*old_cur, link); \
	    RB_LOG("done assigning\n");					\
	    *old_cur = RB_TREE_MATCH_COLOR(*old_cur, succ_node);	\
	  }								\
	  break;							\
	}								\
      }									\
									\
      parent = cur;							\
      cur = next;							\
    }									\
    RB_LOG("Going to return   %p %p %p\n", cur, *cur, ret);		\
    *root = RB_TREE_BLACK(*root);					\
    if (ret) RB_LOG("returned key is %d\n", RB_TREE_KEY(ret, key_field)); \
    return RB_TREE_PTR(ret);						\
  }									\
									\
  for_type *RB_DELETE_MAX(for_type, for_type **cur, for_type **parent) { \
    for_type *ret;							\
    do {								\
      if (RB_TREE_IS_BLACK(*cur)) {					\
	for_type **sibling = (*cur == RB_TREE_LEFT(*parent, link)) ? &RB_TREE_RIGHT(*parent, link) : &RB_TREE_LEFT(*parent, link); \
	if ( RB_TREE_IS_BLACK(*sibling) ) {				\
	  assert (RB_TREE_IS_RED(*parent));				\
	  *parent = RB_TREE_BLACK(*parent);				\
	  *cur = RB_TREE_RED(*cur);					\
	  *sibling = RB_TREE_RED(*sibling);				\
	} else if (*cur == RB_TREE_RIGHT(*parent, link)) {		\
	  *sibling = RB_TREE_BLACK(*sibling);				\
	  *parent = RB_TREE_RED(*parent);				\
	  RB_ROTATE_LEFT(sibling, parent, link);			\
	  parent = &RB_TREE_RIGHT(*parent, link);			\
	  cur = &RB_TREE_RIGHT(*parent, link);				\
	  sibling = &RB_TREE_LEFT(*parent,link);			\
	  *parent = RB_TREE_BLACK(*parent);				\
	  *cur = RB_TREE_RED(*cur);					\
	  *sibling = RB_TREE_RED(*sibling);				\
	} else if (*cur == RB_TREE_LEFT(*parent, link)) {		\
	  *sibling = RB_TREE_BLACK(*sibling);				\
	  *parent = RB_TREE_RED(*parent);				\
	  RB_ROTATE_RIGHT(sibling, parent, link);			\
	  parent = &RB_TREE_LEFT(*parent, link);			\
	  cur = &RB_TREE_LEFT(*parent, link);				\
	  sibling = &RB_TREE_RIGHT(*parent, link);			\
	  *parent = RB_TREE_BLACK(*parent);				\
	  *cur = RB_TREE_RED(*cur);					\
	  *sibling = RB_TREE_RED(*sibling);				\
	} else assert(0);						\
      }									\
    } while (RB_TREE_RIGHT(*cur, link) && (parent = cur) && (cur = &RB_TREE_RIGHT(*cur, link))); \
    ret = *cur;								\
    *cur = NULL;							\
    return ret;								\
  }

#define RB_TREE_LOOKUP(for_type, tree, key) rb_tree_lookup_ ## for_type (tree, key)
#define RB_TREE_INSERT(for_type, root, node) rb_tree_insert_ ## for_type (root, node)
#define RB_TREE_DELETE(for_type, root, key) rb_tree_delete_ ## for_type (root, key)
#define RB_DELETE_MAX(for_type, root, parent) rb_tree_delete_max_ ## for_type (root, parent)

#endif
