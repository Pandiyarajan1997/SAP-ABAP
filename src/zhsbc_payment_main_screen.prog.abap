*----------------------------------------------------------------------*
***INCLUDE ZHSBC_PAYMENT_MAIN_SCREEN.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'HSBC_MAIN'.
  SET TITLEBAR 'HSBC_MAIN'.

  IF G_TREE IS INITIAL.
    " The Tree Control has not been created yet.
    " Create a Tree Control and insert nodes into it.
    PERFORM CREATE_AND_INIT_TREE.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'F_VENEX'.
      CALL SCREEN '101'.
    WHEN 'F_VENRE'.
      CALL SCREEN '102'.
    WHEN 'F_GLEXT'.
      CALL SCREEN '103'.
    WHEN 'F_GLREX'.
      CALL SCREEN '104'.
    when 'REPORT'.
      SUBMIT ZHSBC_PAYMENT_REPORT VIA SELECTION-SCREEN  AND RETURN.
    ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_AND_INIT_TREE .

  DATA: NODE_TABLE TYPE TREEV_NTAB,
        ITEM_TABLE TYPE ITEM_TABLE_TYPE,
        EVENTS TYPE CNTL_SIMPLE_EVENTS,
        EVENT TYPE CNTL_SIMPLE_EVENT.

* create a container for the tree control
  CREATE OBJECT G_CUSTOM_CONTAINER
    EXPORTING      " the container is linked to the custom control with the
         " name 'TREE_CONTAINER' on the dynpro

      CONTAINER_NAME = 'TREE_CON'
    EXCEPTIONS
      CNTL_ERROR = 1
      CNTL_SYSTEM_ERROR = 2
      CREATE_ERROR = 3
      LIFETIME_ERROR = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  IF SY-SUBRC <> 0.
*    MESSAGE A000.
  ENDIF.
* create a list tree
  CREATE OBJECT G_TREE
    EXPORTING
      PARENT              = G_CUSTOM_CONTAINER
      NODE_SELECTION_MODE = CL_GUI_LIST_TREE=>NODE_SEL_MODE_SINGLE
      ITEM_SELECTION     = 'X'
      WITH_HEADERS       = ' '
    EXCEPTIONS
      CNTL_SYSTEM_ERROR           = 1
      CREATE_ERROR                = 2
      FAILED                      = 3
      ILLEGAL_NODE_SELECTION_MODE = 4
      LIFETIME_ERROR              = 5.
  IF SY-SUBRC <> 0.
*    MESSAGE A000.
  ENDIF.

* define the events which will be passed to the backend
                                       " node double click
  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_NODE_DOUBLE_CLICK.
  EVENT-APPL_EVENT = 'X'.                                   "
  APPEND EVENT TO EVENTS.

                                       " item double click
  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_ITEM_DOUBLE_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

                                       " expand no children
  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_EXPAND_NO_CHILDREN.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

                                       " link click
  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_LINK_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

                                       " button click
  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_BUTTON_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

                                       " checkbox change
  EVENT-EVENTID = CL_GUI_LIST_TREE=>EVENTID_CHECKBOX_CHANGE.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  CALL METHOD G_TREE->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS = EVENTS
    EXCEPTIONS
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3.
  IF SY-SUBRC <> 0.
*    MESSAGE A000.
  ENDIF.

CREATE OBJECT G_APPLICATION.

* assign event handlers in the application class to each desired event
  SET HANDLER G_APPLICATION->HANDLE_NODE_DOUBLE_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_ITEM_DOUBLE_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_EXPAND_NO_CHILDREN FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_LINK_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_BUTTON_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_CHECKBOX_CHANGE FOR G_TREE.

* add some nodes to the tree control
* NOTE: the tree control does not store data at the backend. If an
* application wants to access tree data later, it must store the
* tree data itself.

  PERFORM BUILD_NODE_AND_ITEM_TABLE USING NODE_TABLE ITEM_TABLE.

  CALL METHOD G_TREE->ADD_NODES_AND_ITEMS
    EXPORTING
      NODE_TABLE = NODE_TABLE
      ITEM_TABLE = ITEM_TABLE
      ITEM_TABLE_STRUCTURE_NAME = 'MTREEITM'
    EXCEPTIONS
      FAILED = 1
      CNTL_SYSTEM_ERROR = 3
      ERROR_IN_TABLES = 4
      DP_ERROR = 5
      TABLE_STRUCTURE_NAME_NOT_FOUND = 6.
  IF SY-SUBRC <> 0.
*    MESSAGE A000.
  ENDIF.

ENDFORM.                    " CREATE_AND_INIT_TREE
*&---------------------------------------------------------------------*
*&      Form  build_node_and_item_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM BUILD_NODE_AND_ITEM_TABLE
  USING
    NODE_TABLE TYPE TREEV_NTAB
    ITEM_TABLE TYPE ITEM_TABLE_TYPE.

 DATA: NODE TYPE TREEV_NODE,
        ITEM TYPE MTREEITM.

* Build the node table.

* Caution: The nodes are inserted into the tree according to the order
* in which they occur in the table. In consequence, a node must not
* must not occur in the node table before its parent node.

* Node with key 'Root'
  NODE-NODE_KEY = C_NODEKEY-ROOT.
                                       " Key of the node
  CLEAR NODE-RELATKEY.      " Special case: A root node has no parent
  CLEAR NODE-RELATSHIP.                " node.

  NODE-HIDDEN = ' '.                   " The node is visible,
  NODE-DISABLED = ' '.                 " selectable,
  NODE-ISFOLDER = 'X'.                 " a folder.
  CLEAR NODE-N_IMAGE.       " Folder-/ Leaf-Symbol in state "closed":
                                       " use default.
  CLEAR NODE-EXP_IMAGE.     " Folder-/ Leaf-Symbol in state "open":
                                       " use default
  CLEAR NODE-EXPANDER.                 " see below.
  " the width of the item is adjusted to its content (text)
  APPEND NODE TO NODE_TABLE.



* Node with key 'New1'
  CLEAR NODE.
  NODE-NODE_KEY = C_NODEKEY-child1.
  NODE-RELATKEY = C_NODEKEY-root.
  NODE-RELATSHIP = CL_GUI_LIST_TREE=>RELAT_LAST_CHILD.
  APPEND NODE TO NODE_TABLE.



  NODE-NODE_KEY = C_NODEKEY-ROOT1.
                                       " Key of the node
  CLEAR NODE-RELATKEY.      " Special case: A root node has no parent
  CLEAR NODE-RELATSHIP.                " node.

  NODE-HIDDEN = ' '.                   " The node is visible,
  NODE-DISABLED = ' '.                 " selectable,
  NODE-ISFOLDER = 'X'.                 " a folder.
  CLEAR NODE-N_IMAGE.       " Folder-/ Leaf-Symbol in state "closed":
                                       " use default.
  CLEAR NODE-EXP_IMAGE.     " Folder-/ Leaf-Symbol in state "open":
                                       " use default
  CLEAR NODE-EXPANDER.                 " see below.
  " the width of the item is adjusted to its content (text)
  APPEND NODE TO NODE_TABLE.


    CLEAR NODE.
  NODE-NODE_KEY = C_NODEKEY-child2.
  NODE-RELATKEY = C_NODEKEY-root1.
  NODE-RELATSHIP = CL_GUI_LIST_TREE=>RELAT_LAST_CHILD.
  APPEND NODE TO NODE_TABLE.


  NODE-NODE_KEY = C_NODEKEY-ROOT3.
                                       " Key of the node
  CLEAR NODE-RELATKEY.      " Special case: A root node has no parent
  CLEAR NODE-RELATSHIP.                " node.

  NODE-HIDDEN = ' '.                   " The node is visible,
  NODE-DISABLED = ' '.                 " selectable,
  NODE-ISFOLDER = 'X'.                 " a folder.
  CLEAR NODE-N_IMAGE.       " Folder-/ Leaf-Symbol in state "closed":
                                       " use default.
  CLEAR NODE-EXP_IMAGE.     " Folder-/ Leaf-Symbol in state "open":
                                       " use default
  CLEAR NODE-EXPANDER.                 " see below.
  " the width of the item is adjusted to its content (text)
  APPEND NODE TO NODE_TABLE.



* Node with key 'New1'
  CLEAR NODE.
  NODE-NODE_KEY = C_NODEKEY-child3.
  NODE-RELATKEY = C_NODEKEY-root3.
  NODE-RELATSHIP = CL_GUI_LIST_TREE=>RELAT_LAST_CHILD.
  APPEND NODE TO NODE_TABLE.

   NODE-NODE_KEY = C_NODEKEY-mail.
                                       " Key of the node
  CLEAR NODE-RELATKEY.      " Special case: A root node has no parent
  CLEAR NODE-RELATSHIP.                " node.

  NODE-HIDDEN = ' '.                   " The node is visible,
  NODE-DISABLED = ' '.                 " selectable,
  NODE-ISFOLDER = 'X'.                 " a folder.
  CLEAR NODE-N_IMAGE.       " Folder-/ Leaf-Symbol in state "closed":
                                       " use default.
  CLEAR NODE-EXP_IMAGE.     " Folder-/ Leaf-Symbol in state "open":
                                       " use default
  CLEAR NODE-EXPANDER.                 " see below.
  " the width of the item is adjusted to its content (text)
  APPEND NODE TO NODE_TABLE.



* Node with key 'New1'
  CLEAR NODE.
  NODE-NODE_KEY = C_NODEKEY-mail1.
  NODE-RELATKEY = C_NODEKEY-mail.
  NODE-RELATSHIP = CL_GUI_LIST_TREE=>RELAT_LAST_CHILD.
  APPEND NODE TO NODE_TABLE.


* Node with key 'Root'
  CLEAR ITEM.
  ITEM-NODE_KEY = C_NODEKEY-ROOT.
  ITEM-ITEM_NAME = '1'.                " Item with name '1'
  ITEM-CLASS = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT. " Text Item
  " the with of the item is adjusted to its content (text)
  ITEM-ALIGNMENT = CL_GUI_LIST_TREE=>ALIGN_AUTO.
  " use proportional font for the item
  ITEM-FONT = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
  ITEM-TEXT = 'House Bank'(003).
  APPEND ITEM TO ITEM_TABLE.



  CLEAR ITEM.
  ITEM-NODE_KEY = C_NODEKEY-child1.
  ITEM-ITEM_NAME = '1'.
  ITEM-CLASS = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT.
  ITEM-LENGTH = 15.
  ITEM-USEBGCOLOR = 'X'.
  ITEM-TEXT = 'Master Records'.
  APPEND ITEM TO ITEM_TABLE.
*
*


  CLEAR ITEM.
  ITEM-NODE_KEY = C_NODEKEY-ROOT1.
  ITEM-ITEM_NAME = '1'.                " Item with name '1'
  ITEM-CLASS = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT. " Text Item
  " the with of the item is adjusted to its content (text)
  ITEM-ALIGNMENT = CL_GUI_LIST_TREE=>ALIGN_AUTO.
  " use proportional font for the item
  ITEM-FONT = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
  ITEM-TEXT = 'PPC for RTGS'(003).
  APPEND ITEM TO ITEM_TABLE.





  CLEAR ITEM.
  ITEM-NODE_KEY = C_NODEKEY-child2.
  ITEM-ITEM_NAME = '1'.
  ITEM-CLASS = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT.
  ITEM-LENGTH = 14.
  ITEM-USEBGCOLOR = 'X'.
  ITEM-TEXT = 'Purpose Code'.
  APPEND ITEM TO ITEM_TABLE.


 CLEAR ITEM.
  ITEM-NODE_KEY = C_NODEKEY-ROOT3.
  ITEM-ITEM_NAME = '1'.                " Item with name '1'
  ITEM-CLASS = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT. " Text Item
  " the with of the item is adjusted to its content (text)
  ITEM-ALIGNMENT = CL_GUI_LIST_TREE=>ALIGN_AUTO.
  " use proportional font for the item
  ITEM-FONT = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
  ITEM-TEXT = 'Server Connectivity'(003).
  APPEND ITEM TO ITEM_TABLE.



  CLEAR ITEM.
  ITEM-NODE_KEY = C_NODEKEY-child3.
  ITEM-ITEM_NAME = '1'.
  ITEM-CLASS = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT.
  ITEM-LENGTH = 15.
  ITEM-USEBGCOLOR = 'X'.
  ITEM-TEXT = 'SAP to SFTP'.
  APPEND ITEM TO ITEM_TABLE.


 CLEAR ITEM.
  ITEM-NODE_KEY = C_NODEKEY-mail.
  ITEM-ITEM_NAME = '1'.                " Item with name '1'
  ITEM-CLASS = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT. " Text Item
  " the with of the item is adjusted to its content (text)
  ITEM-ALIGNMENT = CL_GUI_LIST_TREE=>ALIGN_AUTO.
  " use proportional font for the item
  ITEM-FONT = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
  ITEM-TEXT = 'E-mail Maintenance'(003).
  APPEND ITEM TO ITEM_TABLE.



  CLEAR ITEM.
  ITEM-NODE_KEY = C_NODEKEY-mail1.
  ITEM-ITEM_NAME = '1'.
  ITEM-CLASS = CL_GUI_LIST_TREE=>ITEM_CLASS_TEXT.
  ITEM-LENGTH = 15.
  ITEM-USEBGCOLOR = 'X'.
  ITEM-TEXT = 'Email'.
  APPEND ITEM TO ITEM_TABLE.


ENDFORM.                               " build_node_and_item_table
