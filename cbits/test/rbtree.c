#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#include "rb_tree.h"
#include "memory.h"

#include "test/test.h"

#define TEST_SIZE 10000

typedef struct test_tree_t {
  RB_TREE_ENTRY(test_tree_t) link;
  uint32_t entry;
} test_tree_t;

DECL_RB_TREE(test_tree_t, uint32_t);

void print_rb_tree(int indent, test_tree_t *root);
int rb_assert(test_tree_t *root);
#define COMPARE_UINT32(a, b) ((a < b) ? (-1) : ((a > b) ? 1 : 0))
RB_TREE_IMPL(test_tree_t, uint32_t, COMPARE_UINT32, entry, link);

#define GENERIC_ASSERT_RB(rb, cond, what, ...)				\
  if (!(cond)) {							\
    fprintf(stderr, "GENERIC_ASSERT:%s:%d:" what "\n", __FILE__, __LINE__ VA_ARGS(__VA_ARGS__)); \
    print_rb_tree(0, rb);						\
    exit(1);								\
  }

int rb_assert(test_tree_t *root)
{
  int lh, rh;

  if ( !root )
    return 1; /* The empty tree is always good! */
  else {
    test_tree_t *left = RB_TREE_LEFT(root, link),
      *right = RB_TREE_RIGHT(root, link);
    int left_black_height, right_black_height;

    assert(RB_TREE_LEFT(root, link) != root);
    assert(RB_TREE_RIGHT(root, link) != root);

    /* Children of red nodes must always be black */
    GENERIC_ASSERT_RB(root, (RB_TREE_IS_RED(root) && !RB_TREE_IS_RED(left)) || RB_TREE_IS_BLACK(root), "root is red and left is red");
    GENERIC_ASSERT_RB(root, (RB_TREE_IS_RED(root) && !RB_TREE_IS_RED(right)) || RB_TREE_IS_BLACK(root), "root is red and right is red");

    /* Binary tree condition, left key must always be <= root key, and right key must be >= */
    GENERIC_ASSERT_RB(root, !left || RB_TREE_KEY(left, entry) <= RB_TREE_KEY(root, entry), "left key (%u) is greater than root key (%u)", RB_TREE_KEY(left, entry), RB_TREE_KEY(root, entry));
    GENERIC_ASSERT_RB(root, !right || RB_TREE_KEY(right, entry) >= RB_TREE_KEY(root, entry), "right key (%u) is less than root key (%u)", RB_TREE_KEY(right, entry), RB_TREE_KEY(root, entry));

    left_black_height = rb_assert(left);
    right_black_height = rb_assert(right);

    GENERIC_ASSERT_RB(root, left_black_height == right_black_height, "left height (%d) and right height (%d) are not equal", left_black_height, right_black_height);

    return RB_TREE_IS_RED(root) ? left_black_height : left_black_height + 1;
  }
}

void print_rb_tree(int indent, test_tree_t *root)
{
  int i = 0;
  for (;i < indent; ++i) printf(" ");
  if (!root)
    printf("LEAF\n");
  else {
    printf("%d (%s) - %p\n", RB_TREE_KEY(root, entry), RB_TREE_IS_RED(root) ? "red" : "black", root);
    assert(RB_TREE_LEFT(root, link) != root);
    assert(RB_TREE_RIGHT(root, link) != root);
    print_rb_tree(indent + 1, RB_TREE_LEFT(root, link));
    print_rb_tree(indent + 1, RB_TREE_RIGHT(root, link));
  }
}

void test_maintain_rb_property()
{
  uint32_t i = 0;
  test_tree_t *root = NULL;

  for (; i < TEST_SIZE; ++i) {
    test_tree_t *new_node = (test_tree_t *) malloc(sizeof(test_tree_t));
    memset(new_node, 0, sizeof(test_tree_t));
    new_node->entry = i;
    GENERIC_ASSERT(RB_TREE_IS_BLACK(new_node), "newly created node is red");
    RB_TREE_INSERT(test_tree_t, &root, new_node);
  }

  for (i = 0; i < TEST_SIZE; ++i)
    GENERIC_ASSERT(RB_TREE_LOOKUP(test_tree_t, root, i), "could not find inserted node");

  rb_assert(root);
}

void test_maintain_rb_property_random()
{
  uint32_t i = 0;
  test_tree_t *root = NULL;
  srand(time(NULL));
  for (; i < TEST_SIZE; ++i) {
    test_tree_t *new_node = (test_tree_t *) malloc(sizeof(test_tree_t));
    memset(new_node, 0, sizeof(test_tree_t));
    new_node->entry = rand();
    GENERIC_ASSERT(RB_TREE_IS_BLACK(new_node), "newly created node is red");
    if (RB_TREE_LOOKUP(test_tree_t, root, new_node->entry)) {
      --i;
      continue;
    }
    RB_TREE_INSERT(test_tree_t, &root, new_node);
    GENERIC_ASSERT(RB_TREE_LOOKUP(test_tree_t, root, new_node->entry) == new_node, "could not find inserted node");
  }

  rb_assert(root);
}

void test_maintain_rb_property_for_delete_random_root()
{
  uint32_t i = 0;
  test_tree_t *root = NULL;
  srand(time(NULL));
  for (; i < TEST_SIZE; ++i) {
    test_tree_t *new_node = (test_tree_t *) malloc(sizeof(test_tree_t));
    memset(new_node, 0, sizeof(test_tree_t));
    new_node->entry = rand();
    if (RB_TREE_LOOKUP(test_tree_t, root, new_node->entry)) {
      --i;
      continue;
    }
    RB_TREE_INSERT(test_tree_t, &root, new_node);
  }

  while (root) {
    RB_TREE_DELETE(test_tree_t, &root, root->entry);
    rb_assert(root);
  }
}

void test_maintain_rb_property_for_delete_random()
{
  uint32_t i = 0;
  test_tree_t *root = NULL;
  uint32_t entries[TEST_SIZE];
  uint8_t was_deleted[TEST_SIZE];
  memset(was_deleted, 0, sizeof(was_deleted));
  srand(time(NULL));
  for (; i < TEST_SIZE; ++i) {
    test_tree_t *new_node = (test_tree_t *) malloc(sizeof(test_tree_t));
    memset(new_node, 0, sizeof(test_tree_t));
    new_node->entry = rand();
    GENERIC_ASSERT(RB_TREE_IS_BLACK(new_node), "newly created node is red");
    if (RB_TREE_LOOKUP(test_tree_t, root, new_node->entry)) {
      --i;
      continue;
    }
    entries[i] = new_node->entry;
    RB_TREE_INSERT(test_tree_t, &root, new_node);
    GENERIC_ASSERT(RB_TREE_LOOKUP(test_tree_t, root, new_node->entry) == new_node, "could not find inserted node");
  }

  for (; i < (TEST_SIZE * 3); ++i) {
    uint32_t deleted_index = rand() % TEST_SIZE;
    uint32_t to_delete = entries[deleted_index];
    test_tree_t *deleted = RB_TREE_DELETE(test_tree_t, &root, to_delete);
    rb_assert(root);
    GENERIC_ASSERT(RB_TREE_IS_BLACK(root), "root must be black");
    if ( was_deleted[deleted_index] ) {
      GENERIC_ASSERT(!deleted, "found a deleted entry (%d)", to_delete);
    } else {
      GENERIC_ASSERT(deleted, "could not find inserted entry (%d) - iteration %d", to_delete, i);
    }
    if ( deleted ) {
      TEST_EQUAL("deleted entry key", "%d", deleted->entry, to_delete);
    }
    GENERIC_ASSERT(!RB_TREE_LOOKUP(test_tree_t, root, to_delete), "found deleted node");

    if ((rand() % 10) == 1) {
      GENERIC_ASSERT(!RB_TREE_DELETE(test_tree_t, &root, to_delete), "deleted node (%d) twice", to_delete);
      rb_assert(root);
    }

    was_deleted[deleted_index] = 1;
  }

  rb_assert(root);
}

void test_maintain_rb_property_for_delete_inorder()
{
  uint32_t i = 0;
  test_tree_t *root = NULL;

  for (; i < TEST_SIZE; ++i) {
    test_tree_t *new_node = (test_tree_t *) malloc(sizeof(test_tree_t));
    memset(new_node, 0, sizeof(test_tree_t));
    new_node->entry = i;
    GENERIC_ASSERT(RB_TREE_IS_BLACK(new_node), "newly created node is red");
    RB_TREE_INSERT(test_tree_t, &root, new_node);
  }

  for (i = 0; i < TEST_SIZE; ++i) {
    test_tree_t *node = RB_TREE_DELETE(test_tree_t, &root, i);
    GENERIC_ASSERT(node, "inserted node was not deleted");
    TEST_EQUAL("removed node key and deleted key", "%d", node->entry, i);
    rb_assert(root);

    GENERIC_ASSERT(!RB_TREE_LOOKUP(test_tree_t, root, i), "removed node was found in the tree (%d)", i);
  }

  rb_assert(root);
  TEST_EQUAL("root and empty tree", "%p", root, NULL);
}

void test_insert_simple()
{
  test_tree_t *root = NULL;

  test_tree_t n1 = { RB_TREE_NODE_INITIALIZER, 1 },
    n2 = { RB_TREE_NODE_INITIALIZER, 2 },
    n3 = { RB_TREE_NODE_INITIALIZER, 3 };

  RB_TREE_INSERT(test_tree_t, &root, &n1);
  TEST_EQUAL("root and first inserted node", "%p", root, &n1);

  RB_TREE_INSERT(test_tree_t, &root, &n2);
  TEST_EQUAL("root and first inserted node", "%p", root, &n1);
  TEST_EQUAL("root->right and second node", "%p", RB_TREE_PTR(RB_TREE_RIGHT(root, link)), &n2);
  GENERIC_ASSERT(RB_TREE_IS_RED(RB_TREE_RIGHT(root, link)), "root->right is not red (%p)",  RB_TREE_RIGHT(root, link));

  RB_TREE_INSERT(test_tree_t, &root, &n3);
  TEST_EQUAL("root and second inserted node", "%p", root, &n2);
  TEST_EQUAL("root->left and first node", "%p", RB_TREE_PTR(RB_TREE_LEFT(root, link)), &n1);
  TEST_EQUAL("root->right and third node", "%p", RB_TREE_PTR(RB_TREE_RIGHT(root, link)), &n3);
  GENERIC_ASSERT(RB_TREE_IS_RED(RB_TREE_LEFT(root, link)), "root->left is not red (%p)",  RB_TREE_LEFT(root, link));
  GENERIC_ASSERT(RB_TREE_IS_RED(RB_TREE_RIGHT(root, link)), "root->left is not red (%p)",  RB_TREE_RIGHT(root, link));
}


void test_delete_case1()
{
  test_tree_t *root = NULL;
  test_tree_t *removed;
  test_tree_t n1 = {RB_TREE_NODE_INITIALIZER, 100},
      n2 = {RB_TREE_NODE_INITIALIZER, 50},
      n3 = {RB_TREE_NODE_INITIALIZER, 150},
      n4 = {RB_TREE_NODE_INITIALIZER, 175},
      n5 = {RB_TREE_NODE_INITIALIZER, 125},
      n6 = {RB_TREE_NODE_INITIALIZER, 75},
      n7 = {RB_TREE_NODE_INITIALIZER, 25},
      n8 = {RB_TREE_NODE_INITIALIZER, 62},
      n9 = {RB_TREE_NODE_INITIALIZER, 87};
  printf("\n==TEST CASE 1==\n");

  root = RB_TREE_BLACK(&n1);
  RB_TREE_LEFT(root, link) = RB_TREE_RED(&n2);
  RB_TREE_RIGHT(root, link) = RB_TREE_BLACK(&n3);
  n3.link.rb_left = RB_TREE_RED(&n5);
  n3.link.rb_right = RB_TREE_RED(&n4);
  n2.link.rb_left = RB_TREE_BLACK(&n7);
  n2.link.rb_right = RB_TREE_BLACK(&n6);
  n6.link.rb_left = RB_TREE_RED(&n8);
  n6.link.rb_right = RB_TREE_RED(&n9);

  print_rb_tree(0, root);
  removed = RB_TREE_DELETE(test_tree_t, &root, 100);
  GENERIC_ASSERT(removed, "did not remove anything");
  GENERIC_ASSERT(removed == &n1, "did not remove the right node. Removed %d\n", removed->entry)
  print_rb_tree(0, root);
  rb_assert(root);
}

void test_delete_case2()
{
  test_tree_t *root = NULL;
  test_tree_t *removed;
  test_tree_t n1 = {RB_TREE_NODE_INITIALIZER, 325},
    n0 = {RB_TREE_NODE_INITIALIZER, 375},
      n2 = {RB_TREE_NODE_INITIALIZER, 275},
      n3 = {RB_TREE_NODE_INITIALIZER, 225},
	n4 = {{RB_TREE_RED(&n1), RB_TREE_RED(&n0)}, 350},
	n5 = {{RB_TREE_RED(&n3), RB_TREE_RED(&n2)}, 250},
      n6 = {RB_TREE_NODE_INITIALIZER, 75},
	n7 = {{RB_TREE_RED(&n6), NULL}, 100},
	  n8 = {{RB_TREE_BLACK(&n5), RB_TREE_BLACK(&n4)}, 300},
      n9 = {{RB_TREE_BLACK(&n7), RB_TREE_RED(&n8)}, 200};
  printf("\n==TEST CASE 2==\n");
  root = &n9;
  print_rb_tree(0, root);
  removed = RB_TREE_DELETE(test_tree_t, &root, 250);
  GENERIC_ASSERT(removed, "did not remove anything");
  GENERIC_ASSERT(removed == &n5, "did not remove the right node. Removed %d\n", removed->entry)
  print_rb_tree(0, root);
  rb_assert(root);
}

/* 1930691097 (red) */
/*  1394888401 (black) */
/*   294504255 (red) */
/*    LEAF */
/*    LEAF */
/*   LEAF */
/*  2091859351 (black) */
/*   LEAF */
/*   LEAF */

void test_delete_case3()
{
  test_tree_t *root = NULL;
  test_tree_t *removed;
  test_tree_t n1 = {RB_TREE_NODE_INITIALIZER, 100},
      n2 = {RB_TREE_NODE_INITIALIZER, 250},
	n3 = {{RB_TREE_RED(&n1), NULL}, 150},
	  n4 = {{RB_TREE_BLACK(&n3), RB_TREE_BLACK(&n2)}, 200};
  printf("\n==TEST CASE 3==\n");
  root = &n4;
  print_rb_tree(0, root);
  removed = RB_TREE_DELETE(test_tree_t, &root, 200);
  GENERIC_ASSERT(removed, "did not remove anything");
  GENERIC_ASSERT(removed == &n4, "did not remove the right node. Removed %d\n", removed->entry)
  print_rb_tree(0, root);
  rb_assert(root);
}

/* 839882121 (black) */
/*  766744511 (black) */
/*   LEAF */
/*   LEAF */
/*  842232476 (black) */
/*   LEAF */
/*   1281610503 (red) */
/*    LEAF */
/*    LEAF */

void test_delete_case4()
{
  test_tree_t *root = NULL;
  test_tree_t *removed;
  test_tree_t n1 = {RB_TREE_NODE_INITIALIZER, 275},
    n2 = {{NULL, RB_TREE_RED(&n1)}, 250},
	n3 = {RB_TREE_NODE_INITIALIZER, 150},
	  n4 = {{RB_TREE_BLACK(&n3), RB_TREE_BLACK(&n2)}, 200};
  printf("\n==TEST CASE 4==\n");
  root = &n4;
  print_rb_tree(0, root);
  removed = RB_TREE_DELETE(test_tree_t, &root, 200);
  GENERIC_ASSERT(removed, "did not remove anything");
  GENERIC_ASSERT(removed == &n4, "did not remove the right node. Removed %d\n", removed->entry)
  print_rb_tree(0, root);
  rb_assert(root);
}

/* 395448673 (black) - 0x7f8363c05090 */
/*  269452385 (black) - 0x7f8363c05030 */
/*   LEAF */
/*   LEAF */
/*  1991443293 (black) - 0x7f8363c050b0 */
/*   1583253366 (red) - 0x7f8363c05011 */
/*    LEAF */
/*    LEAF */
/*   LEAF */

void test_delete_case5()
{
  test_tree_t *root = NULL;
  test_tree_t *removed;
  test_tree_t n1 = {RB_TREE_NODE_INITIALIZER, 225},
    n2 = {{RB_TREE_RED(&n1), NULL}, 250},
	n3 = {RB_TREE_NODE_INITIALIZER, 150},
	  n4 = {{RB_TREE_BLACK(&n3), RB_TREE_BLACK(&n2)}, 200};
  printf("\n==TEST CASE 5==\n");
  root = &n4;
  print_rb_tree(0, root);
  removed = RB_TREE_DELETE(test_tree_t, &root, 200);
  GENERIC_ASSERT(removed, "did not remove anything");
  GENERIC_ASSERT(removed == &n4, "did not remove the right node. Removed %d\n", removed->entry)
  print_rb_tree(0, root);
  rb_assert(root);
}

/*
1205461215 (black) - 0x7fee21c05010
 679971128 (red) - 0x7fee21c04f91 n7
  382968866 (black) - 0x7fee21c04fd0 n4
   LEAF
   549240803 (red) - 0x7fee21c04ff1
    LEAF
    LEAF
  825914707 (black) - 0x7fee21c05030 n3
   693781023 (red) - 0x7fee21c050b1
    LEAF
    LEAF
   LEAF
 1618539572 (black) - 0x7fee21c05070 n8
  1514262609 (red) - 0x7fee21c04fb1
   LEAF
   LEAF
  1961669988 (red) - 0x7fee21c05051
   LEAF
   LEAF
*/

void test_delete_case6()
{
  test_tree_t *root = NULL;
  test_tree_t *removed;
  test_tree_t n1 = {RB_TREE_NODE_INITIALIZER, 75},
      n2 = {RB_TREE_NODE_INITIALIZER, 125},
	n3 = {{RB_TREE_RED(&n2), NULL}, 150},
	n4 = {{NULL, RB_TREE_RED(&n1)}, 50},
	n5 = {RB_TREE_NODE_INITIALIZER, 250},
      n6 = {RB_TREE_NODE_INITIALIZER, 350},
	n7 = {{RB_TREE_BLACK(&n4), RB_TREE_BLACK(&n3)}, 100},
	  n8 = {{RB_TREE_RED(&n5), RB_TREE_RED(&n6)}, 300},
      n9 = {{RB_TREE_RED(&n7), RB_TREE_BLACK(&n8)}, 200};
  printf("\n==TEST CASE 6==\n");
  root = &n9;
  print_rb_tree(0, root);
  removed = RB_TREE_DELETE(test_tree_t, &root, 200);
  GENERIC_ASSERT(removed, "did not remove anything");
  GENERIC_ASSERT(removed == &n9, "did not remove the right node. Removed %d\n", removed->entry)
  print_rb_tree(0, root);
  rb_assert(root);
}

/*
356628055 (black) - 0x7fa86bc05030
 204867808 (red) - 0x7fa86bc04fb1
  152965702 (black) - 0x7fa86bc05010
   LEAF
   LEAF
  220861608 (black) - 0x7fa86bc05050
   LEAF
   LEAF
 796962915 (black) - 0x7fa86bc04fd0
  700206066 (red) - 0x7fa86bc04ff1
   LEAF
   LEAF
  863423783 (red) - 0x7fa86bc05091
   LEAF
   LEAF*/
void test_delete_case7()
{
  test_tree_t *root = NULL;
  test_tree_t *removed;
  test_tree_t n1 = {RB_TREE_NODE_INITIALIZER, 350},
      n2 = {RB_TREE_NODE_INITIALIZER, 250},
	n3 = {RB_TREE_NODE_INITIALIZER, 150},
	n4 = {RB_TREE_NODE_INITIALIZER, 50},
	  n5 = {{RB_TREE_RED(&n2), RB_TREE_RED(&n1)}, 300},
	  n6 = {{RB_TREE_BLACK(&n4), RB_TREE_BLACK(&n3)}, 100},
	n7 = {{RB_TREE_RED(&n6), RB_TREE_BLACK(&n5)}, 200};
  printf("\n==TEST CASE 7==\n");
  root = &n7;
  print_rb_tree(0, root);
  removed = RB_TREE_DELETE(test_tree_t, &root, 200);
  GENERIC_ASSERT(removed, "did not remove anything");
  GENERIC_ASSERT(removed == &n7, "did not remove the right node. Removed %d\n", removed->entry)
  print_rb_tree(0, root);
  rb_assert(root);
}

/*
704677418 (black) - 0x7fd738404f90
 148304378 (red) - 0x7fd738405051 n7
  141051121 (black) - 0x7fd738404fb0 n4
   LEAF
   LEAF
  546750213 (black) - 0x7fd738405030 n3
   LEAF
   690682275 (red) - 0x7fd7384050b1 n2
    LEAF
    LEAF
 1565892714 (red) - 0x7fd738405011 n8
  1012439985 (black) - 0x7fd738404ff0 n6
   LEAF
   1470650526 (red) - 0x7fd738405071 n1
    LEAF
    LEAF
  1971728006 (black) - 0x7fd738404fd0 n5
   1834097159 (red) - 0x7fd738405091 n0
    LEAF
    LEAF
   LEAF
*/
void test_delete_case8()
{
  test_tree_t *root = NULL;
  test_tree_t *removed;
  test_tree_t n1 = {RB_TREE_NODE_INITIALIZER, 275},
    n0 = {RB_TREE_NODE_INITIALIZER, 325},
      n2 = {RB_TREE_NODE_INITIALIZER, 175},
	n3 = {{NULL, RB_TREE_RED(&n2)}, 150},
	n4 = {RB_TREE_NODE_INITIALIZER, 50},
	  n5 = {{RB_TREE_RED(&n0), NULL}, 350},
	  n6 = {{NULL, RB_TREE_RED(&n1)}, 250},
	n7 = {{RB_TREE_BLACK(&n4), RB_TREE_BLACK(&n3)}, 100},
	  n8 = {{RB_TREE_BLACK(&n6), RB_TREE_BLACK(&n5)}, 300},
      n9 = {{RB_TREE_RED(&n7), RB_TREE_RED(&n8)}, 200};
  printf("\n==TEST CASE 8==\n");
  root = &n9;
  print_rb_tree(0, root);
  removed = RB_TREE_DELETE(test_tree_t, &root, 200);
  GENERIC_ASSERT(removed, "did not remove anything");
  GENERIC_ASSERT(removed == &n9, "did not remove the right node. Removed %d\n", removed->entry)
  print_rb_tree(0, root);
  rb_assert(root);
}

void test_delete_with_null_left()
{
  test_tree_t *root = NULL;
  test_tree_t n1 = {RB_TREE_NODE_INITIALIZER, 350},
    n2 = {{NULL, &n1}, 300};
  root = &n1;
  GENERIC_ASSERT(RB_TREE_DELETE(test_tree_t, &root, 200) == NULL, "removed nonexistent");
}


void test_delete_empty_tree()
{
  test_tree_t* root = NULL;
  GENERIC_ASSERT(RB_TREE_DELETE(test_tree_t, &root, 0) == NULL, "Removed nonexistent");
}

int main (int argc, char **argv)
{
  test_delete_case1();
  test_delete_case2();
  test_delete_case3();
  test_delete_case4();
  test_delete_case5();
  test_delete_case6();
  test_delete_case7();
  test_delete_case8();
  test_delete_empty_tree();
  test_delete_with_null_left();

  test_insert_simple();
  test_maintain_rb_property_random();
  test_maintain_rb_property();
  test_maintain_rb_property_for_delete_inorder();
  test_maintain_rb_property_for_delete_random();
  test_maintain_rb_property_for_delete_random_root();

  return 0;
}
