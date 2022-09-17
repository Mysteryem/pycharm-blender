# ***** BEGIN GPL LICENSE BLOCK *****
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# Contributor(s): Campbell Barton, Witold Jaworski
# ***** End GPL LICENSE BLOCK *****
'''
Creates Python predefinition (*.pypredef) files for Blender API
The *.pypredef files are useful for syntax checking and
auto-completion of expressions in Eclipse IDE (with PyDev plugin)

This program is based on Campbell Barton's sphinx_doc_gen.py script

@author: Witold Jaworski (http://www.airplanes3d.net)
'''
#FIXES/UPDATES:
#2012-03-01: In Blender 2.62 the description of the @roll argument in EditBone.transform()
#            method has an unexpected empty line, which break processing. Added handling
#            for that case in process_line() method
#2013-03-01: In Blender 2.66 two methods of the bpy.types.Space obejects are reported as
#            Python - implemented methods, while tehy are not:
#            draw_handler_add() and draw_handler_remove()
#            I have added additional try.. except caluse to hande such errors in the future
#            (However, as long as the descriptions of these methods are wrong, tehy are not documented)
#2013-03-13: Updates  by Brian Henke:
#            * add a no-op (pass) to functions; most were passing because they have comments but the parser fails when there are none.
#            * Remove the "import" class because it is a reserved keyword.
#2013-03-14: Further updates: I have found another function (py_c_func2predef()) which was ommited in
#            the Brian update. I added the "pass" statement generation there.

#2016-June: modifications by Robert Forsman for use with pycharm
#2022-August/September: Updates by Mysteryem:
#            * Add --factory-startup to help message's example usage so that classes/attributes/etc. from addons are not
#              included by default
#            * Add --external-rst-dir to load external module .rst files supporting bgl and bmesh.ops
#            * Recursive submodule inclusion
#              Submodules are placed in the module directory with any of the module's classes etc. going in __init__.py
#            * Replace property assignment to types with type hints and add return type hints
#              Properties that return an object that is both a bpy_struct and another bpy_ base class, e.g.
#              Mesh.vertices are typed using a Union
#              Element types for bpy_prop_array and bpy_prop_collection types are shown via types.Annotate
#              Array property types are formatted according to their dimensions
#            * Add missing bpy_ base classes to bpy.types
#            * Add 'self' and 'cls' arguments to functions
#            * Insert '*' into RNA function arguments when arguments without defaults come after arguments with defaults
#            * Add a no-op (pass) to classes that have no printable functions
#            * Add support for more function/method types
#            * Add correct types for array properties based on the property subtype
#              Some array types return a bpy_prop_array, some return Vector, there are more
#            * Add fake return type hints to bpy.props functions
#              With these type hints, the corresponding property type will be hinted when accessing user defined
#              properties. (The real bpy.props functions return a hidden _PropertyDeferred type)
#            * Include "__" prefixed functions so long as they do not exist on the object type
#            * Add *args to operator parameters for their 3 optional positional arguments
#            * Add fake Generic classes to property types to give typed access to collection and array elements
#            * Copy module .py files if they exist
#            * Include bases in class definitions

# TODO: Operator return type needs to be corrected to set[str]
# TODO: Operators should be callable classes, since they also have functions such as idname and idname_py (they are
#  subclasses of bpy.ops._BPyOpsSubModOp, but that class isn't available as bpy.ops.<anything> returns a module)

script_help_msg = '''
Usage:
- Run this script from blenders root path:

    .\\blender.exe -b --factory-startup -P doc\\python_api\\pypredef_gen.py
  Or with an external rst director specified
    .\\blender.exe -b --factory-startup -P doc\\python_api\\pypredef_gen.py -- --external-rst-dir="<path_to_external_rst>"

  This will generate PyDev python predefiniton files (for Eclipse) in doc\\python_api\\pypredef\\,
  assuming that .\\blender.exe is the blender executable, and you have placed this script in
  .\\doc\\python_api\\ Blender's subdirectory.
  
  Remove --factory_startup if you want to include operators/properties/classes/etc. registered by addons.
  
  Remove -b if you want to include the full bgl module and/or some of the graphics dependent modules/properties/etc.
  
  --external-rst-dir is intended for use with the bgl.rst file in <blender_src_dir>\\doc\\python_api\\rst\\ and the
  bmesh.ops.rst file generated by the running <blender_src_dir>\\doc\\python_api\\rst_from_bmesh_opdefines.py.
  With these two .rst files in the directory specified by --external-rst-dir, the bgl and bmesh.ops modules will
  generate predefinition files with significantly more information. Attempting to use other .rst files isn't supported
  and may not work correctly.
  
  --skip-files or the -s flag can be added after the -- to skip any modules which already exist as files, intended for
  use if you have already added these files as external libraries
'''

# Since the script is set up for use with PyCharm now, this comment may not be correct
'''
Comments to using the pypredef files in Eclipse:
1. Add the directory that contains the *.pypredef files to PYTHONPATH of your project:
    - Open Project-->Properties window.
    - Select PyDev - PYTHONPATH option.
    - In External Libraries tab add this directory to the list.
2. In the same tab (External Libraries) press the button [Force restore internal info]

NOTE: The completion may sometimes appear after a 1-2s. delay.
When you type "." it doesn't, But when you remove it, and type again ".", it appears!

NOTE: In case of specific bpy.app submodule, use it in your code in the following way:

from bpy import app
c = app.build_time

(because plain "import bpy.app" does not work)!
'''

import sys
import builtins

# Switch for quick testing
# select modules to build:
INCLUDE_MODULES = [
    "bpy",
    "bpy_extras",
    "bmesh",
    "aud",
    "bgl",
    "bl_math",
    "bl_ui",
    "blf",
    "mathutils",
    "gpu",
    "gpu_extras",
    "imbuf",
    "freestyle",
    "rna_info",
    "idprop",
]

# Modules to exclude, each module's .__name__ is checked against this set
EXCLUDE_MODULES = {
    "dummy_module_for_formatting_purposes",
}

# Non-module attributes to treat as if they are modules
ATTRIBUTES_AS_SUBMODULES = {
    "bpy": {
        "app",
        # The current context instance tends to have many more attributes than bpy.types.Context, creating a fake module
        # from the current instance will produce predefenitions for many extra attributes
        "context",
    },
}

# Modules/submodules that will include all attributes, initialised to None if the type can't be represented
DOCUMENT_ALL_ATTRIBUTES_MODULES = {
    "bpy",
    "bpy.context",
    "bgl",
}

_BPY_STRUCT_FAKE = "bpy_struct"
_BPY_FULL_REBUILD = False
_IDENT = "    "
_SPECIAL_HANDLING_MODULES = {"bpy.ops", "bpy.types"}
_ARG_SKIP_FILES = False

#dictionary, used to correct some type descriptions:
TYPE_ABERRATIONS = {
        "boolean"   : "bool",
        "integer"   : "int",
        "enum"      : "str",
        "string"    : "str",
        "Matrix"    : "mathutils.Matrix",
        "Vector"    : "mathutils.Vector",
        "Quaternion": "mathutils.Quaternion",
        "Color"     : "mathutils.Color",
        "Euler"     : "mathutils.Euler",
        "subclass of bpy_struct": "bpy.types.bpy_struct",
        "subclass of bpy.types.bpy_struct": "bpy.types.bpy_struct",
        "bpy.types.FCurve or list if index is -1 with an array property.": "bpy.types.FCurve",
        "float triplet": "tuple[float, float, float]",
        "string in ['XYZ', 'XZY', 'YXZ', 'YZX', 'ZXY', 'ZYX']": "Literal['XYZ', 'XZY', 'YXZ', 'YZX', 'ZXY', 'ZYX']",
        "tuple of 2 floats": "tuple[float, float]",
        "mathutils.Vector's": "mathutils.Vector",
        "list of mathutils.Vector's": "list[mathutils.Vector]",
        "tuple, pair of floats": "tuple[float, float]",
        "tuple of mathutils.Vector's": "tuple[mathutils.Vector, mathutils.Vector]",
        "mathutils.Vector or None": "mathutils.Vector",
        "list of strigs": "list[str]",
        "list of strings": "list[str]",
        "FCurve or list if index is -1 with an array property": "FCurve",
        "list of key, value tuples": "list[tuple[str, bpy.types.bpy_struct]]",
        "Undefined (it may change).": "None  # Undefined (it may change)",
        "Matrix Access": "mathutils.MatrixAccess",  # MatrixAccess type is not exposed in the Python API
        "(Vector, float) pair": "(mathutils.Vector, float)",
        "Vector of size 3": "mathutils.Vector  # Size of 3",
        "(Quaternion, float) pair": "(mathutils.Quaternion, float)",
        "Vector or float when 2D vectors are used": "Union[mathutils.Vector, float]  # float when 2D vectors are used",
        "list of tuples": "list[tuple]",
        "list of ints": "list[int]",
        "list of floats": "list[float]",
        "BMElemSeq of BMFace": "bmesh.types.BMElemSeq  # of BMFace",
        "BMElemSeq of BMLoop": "bmesh.types.BMElemSeq  # of BMLoop",
        "BMElemSeq of BMVert": "bmesh.types.BMElemSeq  # of BMVert",
        "BMElemSeq of BMEdge": "bmesh.types.BMElemSeq  # of BMEdge",
        "list of BMLoop tuples": "list[tuple[bmesh.types.BMLoop, bmesh.types.BMLoop, bmesh.types.BMLoop]]",
        "(bmesh.types.BMFace, bmesh.types.BMLoop) pair": "(bmesh.types.BMFace, bmesh.types.BMLoop)",
        "tuple of (bmesh.types.BMFace)": "tuple[(bmesh.types.BMFace), ...]",
        "tuple of bmesh.types.BMVert": "tuple[bmesh.types.BMVert, ...]",
        "tuple of strings": "tuple[str, ...]",
        "tuple pair of functions": "(typing.Callable, typing.Callable)",
        "Interface0D or one of its subclasses.": "Interface0D",
        "List of FEdge objects": "list[_freestyle.FEdge]",
        "List of SVertex objects": "list[_freestyle.SVertex]",
        "list of mathutils.Vector objects": "list[mathutils.Vector]",
        "List of ViewEdge objects": "list[_freestyle.ViewEdge]",
        "str, or None if the ViewShape is not part of a library": "str  # None if the ViewShape is not part of a library ",
        "List of ViewVertex objects": "list[_freestyle.ViewVertex]",
        "pair of floats": "tuple[float, float]",
        "pair of ints": "tuple[int, int]",
        "(list of mathutils.Vector, list of (int, int), list of list of int, list of list of int, list of list of int, list of list of int)":
            "tuple[[mathutils.Vector], [(int, int)], [[int]], [[int]], [[int]], [[int]]]",
        "A tuple pair containing mathutils.Vector or None": "Union[(mathutils.Vector, mathutils.Vector), None]",
        "tuple pair of mathutils.Vector or None if the intersection can't be calculated":
            "Union[(mathutils.Vector, mathutils.Vector), None]  # None if the intersection can't be calculated",
        "tuple of mathutils.Vector's or None when there is no intersection":
            "Union[(mathutils.Vector, mathutils.Vector), None]  # None when there is no intersection",
        "pair of lists": "tuple[list, list]",
        "list of four floats, list of four mathutils.Vector types": "list[list[float], list[mathutils.Vector]]  # list"
                                                                    " of four floats, list of four mathutils.Vector"
                                                                    " types",
        "BMVert, BMEdge or BMFace": "Union[bmesh.types.BMVert, bmesh.types.BMEdge, bmesh.types.BMFace]",
        "dict with string keys": "dict[str, Any]",
        "int or float": "Union[int, float]",
        "(Vector, Quaternion, Vector)": "tuple[mathutils.Vector, mathutils.Quaternion, mathutils.Vector]",
        "ViewEdge or None": "Optional[_freestyle.ViewEdge]",
        "BMVert or None": "Optional[bmesh.types.BMVert]"
}

IGNORED_CLASS_KEYS = {
    '__add_any_other_keys_here__',
}
IGNORED_CLASS_KEYS.update(object.__dict__)

# Return type hints for bpy.props functions
# Note that PyCharm fails to get the correct typing from annotations defined using these bpy.props functions if the
# function hints that it returns a Union of types. This means everything but FloatVectorProperty should get typed
# correctly
_BPY_PROPS_RETURN_HINTS = {
    'BoolProperty': "type[bool]",
    'BoolVectorProperty': "type[bpy.types._generic_prop_array[bool]]",
    'EnumProperty': "type[str]",
    'FloatProperty': "type[float]",
    'FloatVectorProperty': "type[_FloatVectorTypeAmalgamation]",
    'IntProperty': "type[int]",
    'IntVectorProperty': "type[bpy.types._generic_prop_array[int]]",
    'StringProperty': "type[str]",
    # For these we add a T TypeVar and a T type hint to the 'type' parameter
    'PointerProperty': "_T",
    'CollectionProperty': "type[bpy.types._generic_prop_collection_idprop[_T]]",
}

_PROP_ARRAY_SUBTYPE_TO_CLASS = {
    # See bpy_rna.c #define PROP_ALL_VECTOR_SUBTYPES
    'COORDS': 'mathutils.Vector',  # NormalEditModifier.offset
    'TRANSLATION': 'mathutils.Vector',  # HookModifier.center
    'DIRECTION': 'mathutils.Vector',  # MeshLoop.bitangent
    'VELOCITY': 'mathutils.Vector',  # Particle.angular_velocity
    'ACCELERATION': 'mathutils.Vector',  # ClothSettings.gravity
    'XYZ': 'mathutils.Vector',
    'XYZ_LENGTH': 'mathutils.Vector',  # Node.dimensions
    #
    'MATRIX': 'mathutils.Matrix',  # HookModifier.matrix_inverse
    #
    'EULER': 'mathutils.Euler',  # Object.delta_rotation_euler
    #
    'QUATERNION': 'mathutils.Quaternion',  # Object.delta_rotation_quaternion
    #
    'COLOR': 'mathutils.Color',
    'COLOR_GAMMA': 'mathutils.Color',  # Brush.color
    #
    'AXISANGLE': 'bpy_prop_array',  # Object.rotation_axis_angle
    'DISTANCE': 'bpy_prop_array',  # GPencilSculptGuide.location
    'FACTOR': 'bpy_prop_array',  # SplineIKConstraint.joint_bindings
    'LAYER': 'bpy_prop_array',  # Armature.layers_protected
    'LAYER_MEMBER': 'bpy_prop_array',  # Armature.layers
    'NONE': 'bpy_prop_array',  # BoolProperty.default_array
    'PIXEL': 'bpy_prop_array',  # Image.size
    'UNSIGNED': 'bpy_prop_array',  # HookModifier.vertex_indices
}

# Stores mappings from module_name to parsed external rst dict
_EXTERNAL_MODULE_FUNCTION_RST = {}
_EXTERNAL_MODULE_CLASS_RST = {}


import os
import sys
import inspect
import types
import importlib
import bpy
import rna_info
import bmesh
import argparse
import shutil
from typing import Callable
from collections import defaultdict

# BMeshOpFunc type is not exposed directly by the current Blender API
BMeshOpFuncType = type(bmesh.ops.split)
NoneType = types.NoneType if hasattr(types, 'NoneType') else type(None)


def write_indented_lines(ident: str, fn: Callable[[str], None], text: str, strip=True):
    ''' Helper function. Apply same indentation to all lines in a multilines text.
        Details:
        @ident (string): the required prefix (spaces)
        @fn (function): the print() or file.write() function
        @text (string): the lines that have to be shifted right
        @strip (boolean): True, when the lines should be stripped
                          from leading and trailing spaces
    '''
    if text is None:
        return
    for l in text.splitlines():
        if strip:
            fn(ident + l.strip() + "\n")
        else:
            fn(ident + l + "\n")

#Helper functions, that transforms the RST doctext like this:
#   .. method:: from_pydata(vertices, edges, faces)
#
#     Make a mesh from a list of verts/edges/faces
#     Until we have a nicer way to make geometry, use this.
#
#     :arg vertices: float triplets each representing (X, Y, Z) eg: [(0.0, 1.0, 0.5), ...].
#     :type vertices: iterable object
#     :arg edges: int pairs, each pair contains two indices to the *vertices* argument. eg: [(1, 2), ...]
#     :type edges: iterable object
#     :arg faces: iterator of faces, each faces contains three or four indices to the *vertices* argument. eg: [(5, 6, 8, 9), (1, 2, 3), ...]
#     :type faces: iterable object
#
#into pypredef header definition list, which contains following text:
#
#   def from_pydata(vertices, edges, faces):
#       ''' Make a mesh from a list of verts/edges/faces
#           Until we have a nicer way to make geometry, use this.
#           Arguments:
#           @vertices (iterable object): float triplets each representing (X, Y, Z) eg: [(0.0, 1.0, 0.5), ...].
#           @edges (iterable object): int pairs, each pair contains two indices to the *vertices* argument. eg: [(1, 2), ...]
#           @faces (iterable object): iterator of faces, each faces contains three or four indices to the *vertices* argument. eg: [(5, 6, 8, 9), (1, 2, 3), ...]
#       '''
#Some blender built-in functions have nothing, but such formatted docstring (in bpy.props, for example)
def rst2list(doc):
    '''Method tries convert given doctext into list of definition elements
        Arguments:
        @doc (string) - the documentation text of the member (preferably in sphinx RST syntax)
        Returns: dictionary with identified elements of the definition (keys: "@def","@description","@returns", and zero or more function arguments)
                 each dictionary item is a small dictionary, which content depends on the keyword:
                 "@def":
                         "prototype" : function declaration - "<name>([<arg1[,..]])"
                         "description": (optional) general description of the function
                         "type": (optional) type of the returned value - usually for the properties
                 then the list of arguments follows (if it exists)
                 [argument name:]
                         "name": argument's name (just to make the printing easier)
                         "type": argument's type (may be a class name)
                         "description": argument's description
                         "ord": nr kolejny
                 ["@returns":]
                         optional: what function/property returns:
                         "description": description of the content
                         "type":        the name of returned type
                         "ord": nr kolejny
                 ["@note":]
                         optional: note, added to description (below argument list)
                         "description": description of the content
                         "ord": nr kolejny
                 ["@seealso":]
                         optional: reference, added to description (below argument list)
                         "description": description of the content
                         "ord": nr kolejny
    '''
    def process_line(line, definition, last_entry):
        '''Helper function, that analyzes the line and tries to place the
           information it contains into "working definition"
           Arguments:
           @line (string): single line of the description
           @definition (dictionary of dictionaries): working definition of the member
           @last_entry (string): the key in definition, which was used lately (before this call)

           Returns: updated last_entry (string)
        '''
        def type_name(line):
            ''' Helper function, that tries to extract the name of Python type
                Arguments:
                @line (string): text (single line) to analyze (the expression that begins with :type: or :rtype:)
                returns the identified type or None, when it cannot identify it!
            '''
            expr = line.split(" ",1) #split ":type: float" into (':type:','float')
            if len(expr) < 2: return None #we cannot identify it!
            result = expr[1].strip()
            if result in TYPE_ABERRATIONS:
                return TYPE_ABERRATIONS[result]
            else:
                return result

        line = line.lstrip(" ")
        line = line.replace(":class:","").replace("`","") #replace occurences of ":class:`<TypeName>`"
        #                                                  with "<TypeName>"
        line = line.replace(":exc:","").replace("`","") #replace occurences of ":exc:`<TypeName>`"
        #                                                  with "<TypeName>"
        if (
                line.startswith(".. method::")
                or line.startswith(".. function::")
                or line.startswith(".. classmethod::")
                or line.startswith(".. staticmethod::")
        ):
            split = line.split("::", 1)
            prototype = (split[1]).lstrip(" ").rstrip(":")
            function_type = split[0][3:]
            last_entry = "@def"
            definition["@def"].setdefault("prototype", prototype)
            definition["@def"].setdefault("function_type", function_type)
        elif line.startswith(":arg"):
            expr = line.split(" ",2)
            name = expr[1].rstrip(":")
            if len(expr) == 3:
                definition.setdefault(name,{"name":name, "description":expr[2], "ord":len(definition)})
            else:
                definition.setdefault(name,{"name":name, "description":"", "ord":len(definition)})
            last_entry = name
        elif line.startswith(":type:"): #property type
            expr = type_name(line)
            if expr: definition["@def"].setdefault("type",expr)
            last_entry = "@def"
        elif line.startswith(":return:"): #return description
            expr = line.split(" ",1)
            name = "@returns"
            definition.setdefault(name,{"name": "returns", "description":expr[1], "ord":len(definition)})
            last_entry = name
        elif line.startswith(":rtype:"): #type, returned by the function
            expr = type_name(line)
            if last_entry != "@returns": last_entry = "@def"
            if expr: definition[last_entry].setdefault("type",expr)
        elif line.startswith(":type"): #argument's type
            expr = line.split(" ",2)
            name = expr[1].rstrip(":")
            try:
                definition[name].setdefault("type",expr[2])
                last_entry = name
            except:
                print("Missing argument declaration for '%s'" % name)
        elif line.startswith(".. note:: "): #note to member description
            line = line.replace(".. note:: ","")
            name = "@note"
            definition.setdefault(name,{"description":line, "ord":len(definition)})
            last_entry = name
        elif line.startswith(".. seealso::"): #reference to external resource
            line = line.replace(".. seealso:: ","")
            name = "@seealso"
            definition.setdefault(name,{"description":line, "ord":len(definition)})
            last_entry = name
        elif line.startswith(".. literalinclude::"):
            pass #skip this line
        else: #this is just second line of description for the last entry
            #  (whole member, or just an single argument)
            if last_entry in definition and line != "" and not line.startswith("Undocumented"):
                item = definition[last_entry]
                if "description" not in item:
                    item.setdefault("description",line)
                else:
                    item["description"] = item["description"] + line + "\n"
        return last_entry
    #--------------------------------- process_line
    lines = doc.split("\n")
    last_key = "@def"
    definition = {last_key:{"description":"", "ord":0}} #at the beginning: empty description of function definition

    for line in lines:
        last_key = process_line(line,definition,last_key)
    #now let's check the result, stored in <definition> dictionary:
    return definition

def get_item(dictionary,key):
    '''Helper function. Returns the dictionary element at key, or None
        Arguments:
        @dictionary: the dictionary which will be searched
        @key:        the key in the dictionary
    '''
    if key in dictionary:
        return dictionary[key]
    else:
        return None

def rna2list(info):
    ''' Prepares list of definition elements
        Arguments:
        @info (one of rna_info.Info*RNA types) - the descriptor of Struct, Operator, Function or Property
        Returns: dictionary of the same structure, like the one returned by rst2list()
                 "@def":
                         "prototype" : used in structs and functions
                                       for struct: declaration "class AClass(ABaseClass):"
                                       for function or operator: declaration - "<name>([<arg1[,..]])"
                                       for property: declaration - "<name> = <TypeReturned> [# (read only)]"
                         "decorator": (optional) "@classmethod" or "@staticmethod"
                         "description": (optional) general description of the element
                         "hint"       : (optional) formatting hint for the doc2definition() function: "property" for RNA properties, "class" for RNA structs
                 then the list of function's arguments follows (if they exist)
                 [argument name:]
                         "name": argument's name (just to make the printing easier)
                         "type": argument's type (may be a class name)
                         "description": argument's description
                         "ord": ordinal number
                 ["@returns":]
                         optional: what function/property returns:
                         "description": description of the content
                         "type":        the name of returned type
                         "ord": ordinal number (for functions)
                 ["@note":]
                         optional: note, added to description (below argument list)
                         "description": description of the content
                         "ord": ordinal number
                 ["@seealso":]
                         optional: reference, added to description (below argument list)
                         "description": description of the content
                         "ord": oridinal number

    '''
    def type_name(name, include_namespace=False, array_dimensions=None, is_read_only=False, built_in_type=None, collection_element_type=None):
        ''' Helper function, that corrects some wrong type names
            Arguments:
            @name (string): "raw" name, received from RNA
            @include_namespace: True, when append the bpy.types. prefix
            @array_dimensions: int bpy_prop_array specifying dimensions when the type is an array type
            returns the corrected type name (string)
        '''
        # Can be None for a collection type that is elements only
        if name is not None:
            if name in TYPE_ABERRATIONS:
                name = TYPE_ABERRATIONS[name]
            if include_namespace:
                name = "bpy.types." + name

        if built_in_type is None:
            return name
        else:
            if collection_element_type:
                # Start with the fake generic class to give typed access of the collection elements
                names = [f"bpy.types._generic_prop_collection[{collection_element_type}]"]

                if name:
                    # The collection has a base type that can be accessed through .rna_type, append the generic prop
                    # type that gives typed access to .rna_type
                    names.append(f"bpy.types._generic_prop[{name}]")
                    # Append the base type itself
                    names.append(name)

                if built_in_type != 'bpy_prop_collection':
                    # Append the collection type itself, this should always be bpy_prop_collection, though it might be
                    # possible to get a bpy_prop_collection_idprop if addons are loaded
                    names.append(built_in_type)
                if len(names) > 1:
                    # Return a Union of the types
                    return f"Union[{', '.join(names)}]"
                else:
                    return names[0]
            elif array_dimensions is not None:
                array_shape_annotation = name
                at_least_one_dimension_specified = False
                for dim_length in reversed(array_dimensions):
                    if dim_length != 0:
                        if is_read_only:
                            array_shape_annotation = "tuple[" + ", ".join([array_shape_annotation] * dim_length) + "]"
                        else:
                            array_shape_annotation = "list[" + ", ".join([array_shape_annotation] * dim_length) + "]"
                        at_least_one_dimension_specified = True
                if not at_least_one_dimension_specified:
                    # All 0 usually means it is variable, e.g. Image.pixels
                    if is_read_only:
                        array_shape_annotation = "tuple[" + array_shape_annotation + ", ...]"
                    else:
                        array_shape_annotation = "list[" + array_shape_annotation + "]"

                # The array type depends on its subtype
                if built_in_type == "bpy_prop_array":
                    # Array types should also include _generic_prop[type[<property type>]], e.g. bpy.types.FloatProperty
                    # for Image.pixels or bpy.types.IntProperty for Image.size, but as it's the type itself, not an
                    # instance, it's much less useful than with prop_collections
                    # Return our fake generic bpy_prop_array type, annotated to indicate array shape and element
                    # mutability
                    return f"Annotated[bpy.types._generic_prop_array[{name}], {array_shape_annotation}]"
                else:
                    # If it's not a bpy_prop_array, it could be a mathutils.Vector, mathutils.Matrix, mathutils.Euler,
                    # mathutils.Quaternion or mathutils.Color
                    # Annotate the type to indicate array shape and element mutability
                    return f"Annotated[{built_in_type}, {array_shape_annotation}]"

    def get_argitem(arg, prev_ord, is_return=False):
        '''Helper function, that creates an argument definition subdictionary
           Arguments:
           @arg (rna_info.InfoPropertyRNA): descriptor of the argument
           @prev_ord (int): previous order index (to set the value for the "ord" key)

           Returns: an definistion subdictionary (keys: "name", "type", "description", "ord")
        '''
        if arg.fixed_type:
            arg_type = arg.fixed_type.identifier
        else:
            arg_type = arg.type
        if is_return:
            description = arg.get_type_description(as_ret = True) #without default value!
        else:
            description = arg.get_type_description(as_arg = True) #without default value!

        if arg.collection_type == None:
            description = description.replace(arg_type, "", 1) #remove the first occurence of type name - it repeats the declaration!

        if description.startswith(","): #it may happen, when the arg_type was at the begining of the string:
            description = (description[1:]) #skip the leading colon
        if description.startswith(" "):
            description = (description[1:]) #skip first space

        #add some human comments (if it exists):
        if arg.description:
            description = arg.description + "\n" + _IDENT + description

        if is_return:
            return {"name":"returns", "description":description, "type":type_name(arg_type, arg.fixed_type != None), "ord":(prev_ord + 1)}
        else:
            return {"name":arg.identifier, "description":description, "type":type_name(arg_type), "ord":(prev_ord + 1)}

    def get_return(returns, prev_ord):
        '''Helper function, that creates the return definition subdictionary ("@returns")
           Arguments:
           @returns (list of rna_info.InfoPropertyRNA): descriptor of the return values
           @prev_ord (int): previous order index (to set the value for the "ord" key)

           Returns: an definistion subdictionary (keys: type", "description", "ord")
        '''
        if len(returns) == 1:
            return get_argitem(returns[0],prev_ord,is_return = True)
        else: #many different values:
            description = "\n("
            for ret in returns:
                item = get_argitem(ret, prev_ord, is_return = True)
                description = description + "\n{0}{1}({2}):{3}".format(_IDENT, ret.identifier, item.pop("type"), item.pop("description"))
            #give just the description, not the type!
            description = description + "\n)"
            return {"name":"returns", "description":description, "ord":(prev_ord + 1)}

    definition = {"@def":{"description":"", "ord":0}} #at the beginning: empty description of function definition

    if type(info) == rna_info.InfoStructRNA:
        # base class of this struct, if there is no RNA base, its base is bpy_struct
        base_id = info.base.identifier if info.base else "bpy.types.bpy_struct"
        prototype = "class {0}({1}):".format(info.identifier, base_id)
        definition["@def"].setdefault("prototype",prototype)
        definition["@def"]["description"] = info.description
        definition["@def"].setdefault("hint","class")

    elif type(info) == rna_info.InfoPropertyRNA:
        array_dimensions = None
        prop_type = None
        built_in_type = None
        collection_element_type = None
        if info.type == 'collection':
            built_in_type = 'bpy_prop_collection'
            if info.collection_type:
                # The collection has a type itself and may have properties
                prop_type = info.collection_type.identifier
            if info.fixed_type:
                # The element type for the collection
                collection_element_type = "bpy.types." + info.fixed_type.identifier
        elif info.type == 'enum':
            # Technically is bpy_prop, but is only accessible as such from path_resolve(<path>, False)
            # built_in_type = 'bpy_prop'
            if info.enum_items:
                prop_type = "Literal[" + ", ".join("'" + i[0] + "'" for i in info.enum_items) + "]"
            else:
                prop_type = 'str'
            # bpy.types.CacheFile.velocity_unit also has a collection_type (bpy.types.AlembicObjectPaths)
            if info.collection_type:
                print("{} is an enum, but it also has the collection_type {}. Ignoring the collection_type".format(
                        info.identifier, info.collection_type.identifier))
        elif info.type == 'pointer':
            if info.fixed_type:
                # Not sure if props corresponding to PointerProperty can be retrieved as a bpy_prop,
                # path_resolve(<path>, False) doesn't work, so not including the bpy_prop type for now
                prop_type = info.fixed_type.identifier
        elif info.fixed_type:
            print("Unrecognised type '{}' with fixed_type '{}'. Assuming the fixed_type is correct.".format(
                info.type, info.fixed_type
            ))
            prop_type = info.fixed_type.identifier
        else:
            # Technically the built_in_type is bpy_prop for a non-array, but it is only accessible as such from
            # path_resolve(<path>, False) so don't include it as the built_in_type
            # built_in_type = 'bpy_prop'
            prop_type = info.type
            bl_prop = info.bl_prop
            # StringProperties can't be arrays and don't have is_array
            if bl_prop and hasattr(bl_prop, 'is_array'):
                if bl_prop.is_array:
                    if not bl_prop.subtype or bl_prop.subtype not in _PROP_ARRAY_SUBTYPE_TO_CLASS:
                        print("Unknown class for array prop: '{}' with subtype '{}'. Assuming it is bpy_prop_array.".format(info.identifier, bl_prop.subtype))
                        built_in_type = "bpy_prop_array"
                    else:
                        built_in_type = _PROP_ARRAY_SUBTYPE_TO_CLASS[bl_prop.subtype]
                    array_dimensions = bl_prop.array_dimensions

        prototype = "{0}: {1}".format(
            info.identifier,
            type_name(prop_type, info.fixed_type is not None, array_dimensions, info.is_readonly, built_in_type,
                      collection_element_type))
        if info.is_readonly:
            prototype = prototype + "  # (read only)"

        definition["@def"].setdefault("prototype",prototype)
        definition["@def"].setdefault("hint","property")

        if info.description:
            definition["@def"]["description"] = info.description

        definition.setdefault("@returns",{"name" : "returns", "description" : info.get_type_description(as_ret = True), "ord" : 1})

    elif type(info) == rna_info.InfoFunctionRNA:
        # Add the classmethod decorator and append 'cls' to the argument for classmethod and append 'self' to the
        # arguments of functions that use it
        if info.is_classmethod:
            definition["@def"].setdefault("decorator", "@classmethod\n")
            arg_strings = ['cls']
        elif info.bl_func.use_self:
            arg_strings = ['self']
        else:
            arg_strings = []

        # Append each argument with its default value if it exists
        # As we go through the arguments, if we find a required argument (no default value) after a not required
        # argument (has a default value), append '*' before the required argument for the first occurrence of such
        first_not_required = None
        first_required_after_not_required = None
        for i, prop in enumerate(info.args):
            if first_not_required is None:
                if not prop.is_required:
                    first_not_required = i
            elif first_required_after_not_required is None:
                if prop.is_required:
                    first_required_after_not_required = i
                    # The next argument to append doesn't have a default value, but it comes after arguments that do so
                    # '*' must be appended beforehand to signify that all arguments without default values after this
                    # point are required to be specified by name
                    arg_strings.append('*')
            # Append the next argument with its default value if it exists
            arg_strings.append(prop.get_arg_default(force=False))
        args_str = ", ".join(arg_strings)
        prototype = "{0}({1})".format(info.identifier, args_str)
        definition["@def"].setdefault("prototype",prototype)
        definition["@def"].setdefault("hint", "function")
        definition["@def"]["description"] = info.description
        #append arguments:
        for arg in info.args:
            definition.setdefault(arg.identifier, get_argitem(arg,len(definition)))
        #append returns (operators have none):
        if info.return_values:
            definition.setdefault("@returns",get_return(info.return_values,len(definition)))

    elif type(info) == rna_info.InfoOperatorRNA:
        args_str = ", ".join(prop.get_arg_default(force=False) for prop in info.args)
        prototype = "{0}({1})".format(info.func_name, args_str)
        definition["@def"].setdefault("prototype",prototype)
        definition["@def"].setdefault("hint", "operator")
        # definition["@def"].setdefault("decorator","@staticmethod\n")
        if info.description and info.description != "(undocumented operator)":
            definition["@def"]["description"] = info.description
        else: #just empty line
            definition["@def"]["description"] = "undocumented"
        #append arguments:
        for arg in info.args:
            definition.setdefault(arg.identifier, get_argitem(arg,len(definition)))
    else:
        raise TypeError("type was not InfoFunctionRNA, InfoStructRNA, InfoPropertyRNA or InfoOperatorRNA")

    return definition


def rst_module_to_rst_dict(file_contents: str):
    # Skip the first since it will be before the first occurrence of the delimiter
    definitions = file_contents.split("\n.. ")[1:]

    module_rst = None
    function_rsts = []
    class_rsts = []
    for definition in definitions:
        if definition.startswith("module"):
            module_rst = ".. " + definition
            break

    if module_rst is None:
        raise ValueError("Could not find module rst")

    module_line = module_rst[:module_rst.index("\n")]
    module_prefix = ".. module:: "
    module_name = module_line.replace(module_prefix, "")

    for definition in definitions:
        if definition.startswith("class"):
            class_rsts.append(".. " + definition)
        elif definition.startswith("function"):
            function_rsts.append(".. " + definition)

    function_rst_dict = {}
    class_rst_dict = {}

    for function_rst in function_rsts:
        # find function name
        first_line = function_rst[:function_rst.index("\n")]
        try:
            function_prefix = ".. function::"
            function_name = first_line[first_line.index(function_prefix) + 1 + len(function_prefix):first_line.index("(")].strip()

            # Need to modify to any multiline :return: because the existing pypredef code operates line by line
            return_start = function_rst.find(":return:")
            if return_start != -1:
                lines_from_return_to_end = function_rst[return_start:].split("\n")
                return_lines = [lines_from_return_to_end[0]]
                after_lines = []
                found_end = False
                for line in lines_from_return_to_end[1:]:
                    line_stripped = line.lstrip(" ")
                    if line_stripped.startswith(":"):
                        found_end = True
                    if found_end:
                        after_lines.append(line)
                    else:
                        return_lines.append(line)
                function_rst = function_rst[:return_start] + "\\n".join(return_lines) + "\n" + "\n".join(after_lines)

            function_rst_dict[function_name] = function_rst
        except ValueError as ve:
            print("Failed to pass:")
            print(first_line)
            raise ve

    for class_rst in class_rsts:
        # find class name
        first_line = class_rst[:class_rst.index("\n")]
        class_prefix = ".. class::"
        class_name = first_line[class_rst.index(class_prefix) + 1 + len(class_prefix):class_rst.index("\n")].strip()
        class_rst_dict[class_name] = class_rst

    return module_name, module_rst, function_rst_dict, class_rst_dict


def doc2definition(doc,docstring_ident=_IDENT, module_name=None):
    '''Method converts given doctext into declaration and docstring comment
    Details:
    @doc (string or list) - the documentation text of the member (preferably in sphinx RST syntax)
                            or ready dictionary of dictionaries, like the result of rst2list() (see above)
    @docstring_ident (string) - the amount of spaces before docstring markings
    @function - function, that should be used to get the list
    Returns: dictionary with following elements:
             "declaration": function declaration (may be omitted for attributes docstrings)
             "docstring": properly idented docstring (leading and trailing comment markings included)
             "returns": type, returned by property/function (to use in eventual return statement)
    '''
    def pop(definition,key):
        '''Removes the given element form the dictionary
            Arguments:
            @definition: dictionary[key]
            @key:        the key in the definition dictionary
        '''
        if key in definition:
            return definition.pop(key)
        else:
            return None

    def format_arg(data):
        '''Returns line of text, describing an argument or return statement
            Arguments:
            data (dictionary): a "subdictionary" of <definition>, describing single item:
                                ("ord", "name", ["description"],["type"])
        '''
        if "type" in data and "description" in data:
            return "@{name} ({type}): {description}".format(**data)
        elif "type" in data:
            return "@{name} ({type}): <not documented>".format(**data)
        elif "description" in data:
            return "@{name}: {description}".format(**data)
        else:
            return "@{name}: <not documented>".format(**data)

    def get(definition,key,subkey):
        ''' Returns the given value from the definition dictionary, or None
            when it does not exists
            Arguments:
            @definition: dictionary[key] of dictionaries[subkey]
            @key:        the key in the definition dictionary
            @subkey:     the key in the definition[key] subdictionary
        '''
        if key in definition:
            if subkey in definition[key]:
                return definition[key][subkey]
            else:
                return None
        else:
            return None

    def guess_prefixed_type(type_name):
        if module_name and type_name.isalnum() and not hasattr(builtins, type_name):
            return f"{module_name}.{type_name}"
        else:
            # Most likely already contains the correct module
            return type_name

    if doc is None:
        return {"docstring" : docstring_ident + "\n"}

    if type(doc) is str :
        definition = rst2list(doc)
    else:
        definition = doc #assume, that doc is the ready definition list!

    rtype = get(definition,"@def","type")
    if rtype is None:
        rtype = get(definition,"@returns","type") #for functions

    _returns = pop(definition, "@returns")

    _note = pop(definition,"@note")

    _seealso = pop(definition, "@seealso")

    declaration = get(definition, "@def","prototype")
    # function, method, classmethod or staticmethod
    function_type = get(definition, "@def", "function_type")
    decorator = get(definition, "@def", "decorator")
    hint = get(definition, "@def", "hint")

    if declaration:
        decorator = decorator if decorator else ""
        if hint in ("property", "class"):
            if hint == "class":
                # If class has bases, try to guess the prefixed name for each base
                bases_start = declaration.find('(')
                if bases_start != -1:
                    bases_end = declaration.rfind(')')
                    if bases_end != -1 and bases_end > bases_start:
                        bases = declaration[bases_start+1:bases_end].split(',')
                        bases = map(guess_prefixed_type, bases)
                        declaration = declaration[:bases_start+1] + ", ".join(bases) + declaration[bases_end:]
            pass #no prefix needed
        else:
            if rtype:
                # Some type aberrations have comments, which will need to go after the ":" at the end
                comment_start = rtype.find("#")
                if comment_start != -1:
                    rtype_stripped = rtype[:comment_start].strip()
                    rtype_stripped = guess_prefixed_type(rtype_stripped)
                    rtype_comment = rtype[comment_start:]
                    declaration = decorator + "def " + declaration + " -> " + rtype_stripped + ":  " + rtype_comment
                else:
                    rtype = guess_prefixed_type(rtype.strip())
                    declaration = decorator + "def " + declaration + " -> " + rtype + ":"
            else:
                declaration = decorator + "def " + declaration + ":"
    elif rtype:
        rtype = guess_prefixed_type(rtype)


    _def = pop(definition, "@def") #remove the definition from the list....

    ident = docstring_ident + _IDENT #all next row will have additional ident, to match the first line
    lines = [] #lines of the docstring text

    al = lines.append #trick, to re-use the write_indented_lines to add the line

    if "description" in _def:
        write_indented_lines(ident,al,_def["description"],False) #fill the <lines> list
        if lines:
            lines[0] = lines[0][len(ident):] #skip the ident in the first and the last line:
            #                                 (the docstring's prefix "   '''" will be placed there)

    if definition.keys(): #Are named arguments there?
        # Disabled, it seems to break docstings from showing in PyCharm's Quick Documentation
        #write_indented_lines(ident,al,"Arguments:",False)

        for tuple in sorted(definition.items(),key = lambda item: item[1]["ord"]): #sort the lines in the original sequence
            #first item of the <tuple> is the key, second - the value (dictionary describing a single element)
            write_indented_lines(ident,al,format_arg(tuple[1]),False)
        #end for
        al("\n")

    if _returns:
            write_indented_lines(ident,al,format_arg(_returns),False)

    if _note and "description" in _note:
        write_indented_lines(ident,al,"Note: " + _note["description"],False)

    if _seealso and "description" in _seealso:
        write_indented_lines(ident,al,"(seealso " + _seealso["description"]+")\n",False)

    if not lines:
        lines.append("<not documented>\n")

    result = {"docstring" : docstring_ident + "\"\"\"" + "".join(lines)+ docstring_ident  + "\"\"\"\n"}

    if declaration:
        result.setdefault("declaration",declaration)

    if rtype:
        result.setdefault("returns",rtype)

    if function_type:
        result.setdefault("function_type", function_type)

    return result

def pyfunc2predef(ident, fw, identifier, py_func, attribute_defined_class=None):
    ''' Creates declaration of a function or class method
        Details:
        @ident (string): the required prefix (spaces)
        @fw (function): the unified shortcut to print() or file.write() function
        @identifier (string): the name of the member
        @py_func (<py function>): the method, that is being described here
        @is_class (boolean): True, when it is a class member
    '''
    is_class = attribute_defined_class is not None
    function_defined_class = getattr(py_func, '__self__', attribute_defined_class)
    try:
        arguments = inspect.getfullargspec(py_func)
        if is_class:
            if len(arguments.args) == 0:
                fw(ident + "@staticmethod\n")
            else:
                static_attr = inspect.getattr_static(function_defined_class, identifier)
                if isinstance(static_attr, staticmethod):
                    fw(ident + "@staticmethod\n")
                elif isinstance(static_attr, classmethod):
                    fw(ident + "@classmethod\n")

        definition = doc2definition(py_func.__doc__) #parse the eventual RST sphinx markup

        if "declaration" in definition:
            write_indented_lines(ident,fw, definition["declaration"],False)
        else:
            arg_str = inspect.formatargspec(*arguments)
            # Set or List default arguments can end up like:
            #   types={<class 'Stroke'>, <class 'StrokeVertexIterator'>}
            arg_str = arg_str.replace("<class '", "").replace("'>", "")
            fmt = ident + "def %s%s"
            if "returns" in definition:
                returns = definition["returns"]
                # Some type amalgamations have comments, move them after the ":" at the end
                comment_start = returns.find("#")
                if comment_start != -1:
                    returns_stripped = returns[:comment_start].rstrip()
                    returns_comment = returns[comment_start:]
                    fmt += " -> " + returns_stripped + ":  " + returns_comment + "\n"
                else:
                    fmt += " -> " + returns + ":\n"
            else:
                fmt += ":\n"
            fw(fmt % (identifier, arg_str))

        if "docstring" in definition:
            write_indented_lines(ident,fw,definition["docstring"],False)

        if "returns" in definition:
            write_indented_lines(ident+_IDENT, fw, "return ...", False)
        else:
            write_indented_lines(ident+_IDENT,fw,"pass",False)

        fw(ident + "\n")
        return True
    except Exception as ex:
        msg = "#unable to describe the '%s' method due to internal error" % identifier
        print(msg + ":")
        print(ex)
        fw(ident + msg + "\n\n")
        return False

def py_descr2predef(ident, fw, descr, module_name, type_name, identifier):
    ''' Creates declaration of a function or class method
        Details:
        @ident (string): the required prefix (spaces)
        @fw (function): the unified shortcut to print() or file.write() function
        @descr(<type descriptor>): an object, describing the member
        @module_name (string): the name of this module
        @type_name (string): the name of the containing class
        @identifier (string): the name of the member
    '''

    if identifier.startswith("_"):
        return

    if type(descr) in (types.GetSetDescriptorType, types.MemberDescriptorType): #an attribute of the module or class
        definition = doc2definition(descr.__doc__,"", module_name=module_name) #parse the eventual RST sphinx markup
        if "returns" in definition:
            returns = definition["returns"]
        else:
            returns = "None"    #we have to assign just something, to be properly parsed!

        fw(ident + identifier + " : " + returns + "\n")

        if "docstring" in definition:
            write_indented_lines(ident,fw,definition["docstring"],False)

    elif type(descr) in (types.MethodDescriptorType, types.ClassMethodDescriptorType):
        py_c_func2predef(ident,fw,module_name,type_name,identifier,descr,True)
    else:
        raise TypeError("type was not MemberDescriptiorType, GetSetDescriptorType, MethodDescriptorType or ClassMethodDescriptorType")
    fw("\n")


def py_c_func2predef(ident, fw, module_name, type_name, identifier, py_func, is_class=True):
    ''' Creates declaration of a function or class method
        Details:
        @ident (string): the required prefix (spaces)
        @fw (function): the unified shortcut to print() or file.write() function
        @type_name (string): the name of the class
        @py_func (<py function>): the method, that is being described here
        @is_class (boolean): True, when it is a class member
    '''
    if module_name in _EXTERNAL_MODULE_FUNCTION_RST:
        funct_dict = _EXTERNAL_MODULE_FUNCTION_RST[module_name]
        if identifier in funct_dict:
            doc = funct_dict[identifier]
        else:
            doc = py_func.__doc__
    else:
        doc = py_func.__doc__
    definition = doc2definition(doc, module_name=module_name) #parse the eventual RST sphinx markup
    function_type = definition.get("function_type")
    is_class_method = type(py_func) == types.ClassMethodDescriptorType or function_type == 'classmethod'
    if is_class_method:
        fw(ident+"@classmethod\n")
    is_static_method = function_type == 'staticmethod'
    if is_static_method:
        assert not is_class_method
        fw(ident + "@staticmethod\n")

    if "declaration" in definition:
        declaration = definition["declaration"]
        if is_class:
            # Insert cls or self parameter
            if "returns" in definition:
                no_args = "()->" in declaration.replace(" ", "")
            else:
                no_args = "():" in declaration.replace(" ", "")
            if is_class_method:
                if no_args:
                    declaration = declaration.replace("(", "(cls", 1)
                else:
                    declaration = declaration.replace("(", "(cls, ", 1)
            elif not is_static_method:
                if no_args:
                    declaration = declaration.replace("(", "(self", 1)
                else:
                    declaration = declaration.replace("(", "(self, ", 1)

        elif module_name == 'bpy.props' and hasattr(py_func, '__name__') and py_func.__name__ in _BPY_PROPS_RETURN_HINTS:
            # This is a hack to add in type hints to bpy.props, it relies on extra typing imports added into the module
            if py_func.__name__ == 'PointerProperty':
                declaration = declaration.replace("type=None", "type: _T = None")
            elif py_func.__name__ == 'CollectionProperty':
                declaration = declaration.replace("type=None", "type: type[_T] = None")
            declaration = declaration.replace("):", ") -> " + _BPY_PROPS_RETURN_HINTS[py_func.__name__] + ":")

        write_indented_lines(ident, fw, declaration, False)
    else:
        # *argv, when we do not know about its arguments
        arg_str = "(*argv)"
        try:
            # getfullargspec can parse .__text_signature__ if present
            arg_str = inspect.formatargspec(*inspect.getfullargspec(py_func))
        except TypeError:
            pass
        if "returns" in definition:
            fw(f"{ident}def {identifier}{arg_str} -> {definition['returns']}:\n")
        else:
            fw(f"{ident}def {identifier}{arg_str}:\n")

    if "docstring" in definition:
        write_indented_lines(ident,fw,definition["docstring"],False)

    if "returns" in definition:
        # write_indented_lines(ident+_IDENT,fw,"return " + definition["returns"],False)
        write_indented_lines(ident+_IDENT,fw,"return ...", False)
    else:
        write_indented_lines(ident+_IDENT,fw,"pass",False)

    fw(ident + "\n")


def pyprop2predef(ident, fw, identifier, py_prop):
    ''' Creates declaration of a property
        Details:
        @ident (string): the required prefix (spaces)
        @fw (function): the unified shortcut to print() or file.write() function
        @identifier (string): the name of the property
        @py_prop (<py property>): the property, that is being described here
    '''
    definition = doc2definition(py_prop.__doc__,"") #parse the eventual RST sphinx markup
    if "returns" in definition:
        declaration = identifier + " = " + definition["returns"]
    else:
        declaration = identifier + " = None"    #we have to assign just something, to be properly parsed!

    # readonly properties use "data" directive, variables use "attribute" directive
    if py_prop.fset is None: declaration = declaration + " # (readonly)"

    fw(ident + declaration + "\n")

    if "docstring" in definition:
        write_indented_lines(ident, fw, definition["docstring"], False)

    fw(ident + "\n")


def guess_prefixed_class_name(module_name, clazz):
    # Overrides for some modules
    module_overrides = {"bpy_types": "bpy.types"}
    module = module_overrides.get(clazz.__module__, clazz.__module__)
    if module == 'builtins':
        # Classes from C may say their module is 'builtins', check that against the actual 'builtins' module
        if hasattr(builtins, clazz.__qualname__):
            # The class is an actual 'builtin' so don't include the module
            return clazz.__qualname__
        else:
            # The class is some class from C, we will guess that the class belongs to the current module as
            # we don't have anything else to go on
            return f"{module_name}.{clazz.__qualname__}"
    else:
        return f"{module}.{clazz.__qualname__}"


def pyclass2predef(fw, module_name, type_name, value):
    ''' Creates declaration of a class
        Details:
        @fw (function): the unified shortcut to print() or file.write() function
        @module_name (string): the name of the module, that contains this class
        @type_name (string): the name of the class
        @value (<class type>): the descriptor of this type
    '''
    if value.__bases__ == (object,):
        fw("class %s:\n" % type_name)
    else:
        base_names = [guess_prefixed_class_name(module_name, base) for base in value.__bases__]
        fw(f"class {type_name}({', '.join(base_names)}):\n")
    if module_name in _EXTERNAL_MODULE_CLASS_RST:
        class_rst_dict = _EXTERNAL_MODULE_CLASS_RST[module_name]
        if type_name in class_rst_dict:
            doc = class_rst_dict[type_name]
        else:
            doc = value.__doc__
    else:
        doc = value.__doc__
    definition = doc2definition(doc, module_name=module_name) #parse the eventual RST sphinx markup
    if "docstring" in definition:
        write_indented_lines("", fw, definition["docstring"], False)

    descr_items = [(key, descr) for key, descr in sorted(value.__dict__.items()) if key not in IGNORED_CLASS_KEYS]

    # Write in the same order as the lists so that similar types are grouped together
    py_wrapper_descriptors = []
    py_class_method_descriptors = []
    py_method_descriptors = []
    py_functions = []
    py_getset_descriptors = []
    py_properties = []
    py_member_descriptors = []
    py_c_functions = []
    class_attributes = []
    for key, descr in descr_items:
        # Not sure if this is correct to do, but get the corresponding function when we get a staticmethod or
        # classmethod object
        if isinstance(descr, staticmethod):
            descr = getattr(value, key)
        elif isinstance(descr, classmethod):
            descr = getattr(value, key)

        if isinstance(descr, types.ClassMethodDescriptorType):
            py_class_method_descriptors.append((key, descr))
        elif isinstance(descr, types.MethodDescriptorType):
            py_method_descriptors.append((key, descr))
        elif isinstance(descr, (types.FunctionType, types.MethodType)):
            py_functions.append((key, descr))
        elif isinstance(descr, types.GetSetDescriptorType):
            py_getset_descriptors.append((key, descr))
        elif isinstance(descr, property):
            py_properties.append((key, descr))
        elif isinstance(descr, types.MemberDescriptorType):
            py_member_descriptors.append((key, descr))
        elif isinstance(descr, (types.BuiltinFunctionType, types.BuiltinMethodType)):
            py_c_functions.append((key, descr))
        elif isinstance(descr, types.WrapperDescriptorType):
            py_wrapper_descriptors.append((key, descr))
        elif isinstance(descr, (bool, int, float, str, bytes, NoneType)):
            class_attributes.append((key, descr))
        else:
            print("\tnot documenting {}.{} of type {}".format(type_name, key, type(descr)))

    for key, descr in class_attributes:
        fw(f"{_IDENT}{key} = {repr(descr)}\n\n")

    # pyfunc2predef can fail, in which case, only a comment is printed
    py_wrapper_descriptors_written = False
    for key, descr in py_wrapper_descriptors:
        py_wrapper_descriptors_written |= pyfunc2predef(_IDENT, fw, key, descr, attribute_defined_class=value)

    for key, descr in py_class_method_descriptors:
        py_descr2predef(_IDENT, fw, descr, module_name, type_name, key)

    for key, descr in py_method_descriptors:
        py_descr2predef(_IDENT, fw, descr, module_name, type_name, key)

    # pyfunc2predef can fail, in which case, only a comment is printed
    py_functions_written = False
    for key, descr in py_functions:
        py_functions_written |= pyfunc2predef(_IDENT, fw, key, descr, attribute_defined_class=value)

    for key, descr in py_getset_descriptors:
        py_descr2predef(_IDENT, fw, descr, module_name, type_name, key)

    for key, descr in py_properties:
        pyprop2predef(_IDENT, fw, key, descr)

    for key, descr in py_member_descriptors:
        py_descr2predef(_IDENT, fw, descr, module_name, "", key)

    for key, descr in py_c_functions:
        py_c_func2predef(_IDENT, fw, module_name, type_name, key, descr, is_class=True)

    all_printed = [py_wrapper_descriptors_written, py_class_method_descriptors, py_method_descriptors,
                   py_functions_written, py_getset_descriptors, py_properties, py_member_descriptors, py_c_functions,
                   class_attributes]
    if not any(all_printed):
        # Add "pass" if nothing was found for writing
        write_indented_lines(_IDENT, fw, "pass", False)

    fw("\n\n")


def pymodule2predef(BASEPATH, module_name, module, title, visited, parent_name=None):
    # The path taken to get to a module may be different to what it reports as its name, and sometimes we treat a class
    # like a module
    is_fake_module = not isinstance(module, types.ModuleType)
    relative_name = (parent_name + "." + module_name) if parent_name else module_name
    true_module_name = relative_name if is_fake_module else module.__name__
    if true_module_name in visited:
        # Usually shouldn't happen since we're generally limiting submodules to those with names that start with the
        # name of their parent module. Better to be safe than to run into infinite recursion issues.
        print("- Already visited '{}'. Skipping.".format(true_module_name))
    else:
        visited.add(true_module_name)

    module_type = "submodule" if parent_name else "module"
    if true_module_name in EXCLUDE_MODULES:
        print("- Skipping {} '{}'".format(module_type, true_module_name))
        return
    else:
        print("- Processing {} '{}'".format(module_type, true_module_name))

    #list submodules:
    submodules = {}

    attributes_processed_as_modules = set()

    # __all__, when used, can help identify submodules that won't appear in dir(module) until they are imported for the
    # first time
    if hasattr(module, '__all__'):
        all_attribute_names = set(module.__all__).union(dir(module))
    else:
        all_attribute_names = dir(module)

    for attribute_name in sorted(all_attribute_names):
        if not attribute_name.startswith("_"):
            if not hasattr(module, attribute_name):
                # It may be a module specified by __all__ that hasn't been loaded, try importing it
                try:
                    value = importlib.import_module(f"{relative_name}.{attribute_name}")
                except Exception as e:
                    print(f"Found attribute '{relative_name}.{attribute_name}' that could not be found on"
                          f" '{relative_name}' and could not be loaded as a module due to: {e}")
                    continue
            else:
                value = getattr(module, attribute_name)

            # Check if the attribute is a module
            if isinstance(value, types.ModuleType):
                # Check that the submodule name starts with the name of the module, this is so that submodules such as
                # bpy_extras.asset_utils.bpy are ignored
                if (
                        value.__name__.startswith(relative_name)
                        # msgbus is an exception to Blender's usual naming conventions for submodules
                        or (relative_name == 'bpy' and value.__name__ == 'msgbus' and attribute_name == 'msgbus')
                        # Doesn't follow naming conventions and is also available as freestyle.chainingiterators.CF,
                        # freestyle.functions.CF and freestyle.shaders.CF, where it will be skipped
                        or (relative_name == 'freestyle.utils' and attribute_name == 'ContextFunctions')
                ):
                    submodules[attribute_name] = value
                else:
                    print("Found possibly misplaced submodule '{}' as {}.{}. Skipping".format(value.__name__, relative_name, attribute_name))
            elif relative_name in ATTRIBUTES_AS_SUBMODULES and attribute_name in ATTRIBUTES_AS_SUBMODULES[relative_name]:
                submodules[attribute_name] = value
                attributes_processed_as_modules.add(attribute_name)

    if submodules:
        dir_path = os.path.join(BASEPATH, module_name)
        if not os.path.isdir(dir_path):
            os.mkdir(dir_path)
        filepath = os.path.join(dir_path, "__init__.py")

        for attribute_name, submodule in submodules.items():
            # Recursive call for each submodule
            title_type = "Submodule" if isinstance(submodule, types.ModuleType) else "Fake Submodule"
            pymodule2predef(
                dir_path,
                module_name=attribute_name,
                module=submodule,
                title="{} {}.{}".format(title_type, relative_name, attribute_name),
                visited=visited,
                parent_name=relative_name)
    else:
        filepath = os.path.join(BASEPATH, module_name + ".py")

    # If the module has a __file__ we can simply copy the corresponding file (or skip it, meant for when the user
    # already has the file added as an external library and doesn't want it duplicated)
    # We ignore 'bpy' as it specifically tries to import c-defined submodules from _bpy, the C-defined module, but we
    # need it to set up our fake/stubbed modules
    if hasattr(module, '__file__') and true_module_name != 'bpy':
        if _ARG_SKIP_FILES:
            print(f"- {true_module_name} already exists as a file. Skipping it.")
        else:
            print(f"- {true_module_name} already exists as a file. Copying the file")
            # The module already exists as a file, copy it
            shutil.copy(module.__file__, filepath)
        return

    attribute_set = set()

    file = open(filepath, "w")
    fw = file.write

    #The description of this module:
    if module.__doc__:
        title = title + "\n" + module.__doc__
    definition = doc2definition(title,"", module_name=true_module_name) #skip the leading spaces at the first line...
    fw(definition["docstring"])
    fw("\n\n")

    # Figure out import depth needed for relative imports to top-level modules
    depth = 0
    if submodules:
        depth += 1 + relative_name.count('.')
    else:
        depth += relative_name.count('.')
    if depth > 0:
        import_str = f"from .{'.' * depth} import"
    else:
        import_str = "import"

    fw(f"import numpy  # added by pypredef_gen\n")
    # We try to represent PyStructSequence types as namedtuples
    fw(f"from collections import namedtuple  # added by pypredef_gen\n")
    # Classes can often be referenced before they are defined so their names are usually prefixed with the module they
    # belong to, for this to work, each module needs to import their top-level module
    top_level_module = relative_name.split('.', maxsplit=1)[0]
    fw(f"{import_str} {top_level_module}  # added by pypredef_gen\n")
    # It's too much work to figure out when we do/don't need to import bpy and mathutils, so always do it unless it's
    # the top_level_module we just imported
    if top_level_module != 'bpy':
        fw(f"{import_str} bpy  # added by pypredef_gen\n")
    if top_level_module != 'mathutils':
        fw(f"{import_str} mathutils  # added by pypredef_gen\n")

    # Extra typing imports needed for some types and type aberrations
    fw("from typing import Union, Optional, Literal, Any  # added by pypredef_gen\n")

    if relative_name == 'bpy.props':
        # Extra imports and TypeVar creation
        # sys is used in default arguments for 'min' and 'max' parameters
        fw("import sys"
           "\nfrom typing import TypeVar, Annotated  # added by pypredef_gen"
           "\n_T = TypeVar('_T')  # added by pypredef_gen\n\n"
           "class _FloatVectorTypeAmalgamation(bpy.types._generic_prop_array[float], mathutils.Vector,"
           " mathutils.Matrix, mathutils.Euler, mathutils.Quaternion, mathutils.Color):"
           "\n    \"\"\"Fake class added by pypredef_gen to work around PyCharm failing to correctly type annotations"
           " that are set by a function that returns a Union of types\"\"\""
           "\n    pass\n")

    # Separator for end of module imports
    fw("\n")

    # Add submodule imports to help with resolving names
    for submodule_name in submodules:
        fw("from . import {}\n".format(submodule_name))
    if submodules:
        fw("\n")

    if relative_name == 'bpy.app':
        fake_persistent_class = (
"""from inspect import isfunction


class _persistent:
    \"\"\"Fake class added by py_predefgen, roughly imitating the behaviour of bpy.app.handlers.persistent\"\"\"
    def __init__(self, func):
        if isfunction(func):
            func._bpy_persistent = None
        else:
            raise ValueError("bpy.app.handlers.persistent expected a function")


"""
        )
        fw(fake_persistent_class)

    if is_fake_module and isinstance(module, bpy.types.bpy_struct):
        structs, _, _, _ = get_rna_info()
        key = ('', module.bl_rna.name)
        fw("# Start of attributes from type #\n")
        if key in structs:
            cls = structs[key]
            attribute_set.update(rna_struct2predef("", fw, cls, is_fake_module=True, module_name=relative_name))
        fw("# End of attributes from type #\n\n")

        descr_items = [(key, descr) for key, descr in sorted(type(module).__dict__.items()) if key not in IGNORED_CLASS_KEYS]

        for key, descr in descr_items:
            # Write all the extra __<name>__ functions that are defined on this class
            if isinstance(descr, types.WrapperDescriptorType):
                pyfunc2predef("", fw, key, descr, attribute_defined_class=type(module))

        # write members of the module
        # only tested with PyStructs which are not exactly modules
        # List the properties, first:
        for key, descr in descr_items:
            # naughty, we also add getset's into PyStructs, this is not typical py but also not incorrect.
            if type(descr) == types.GetSetDescriptorType :  # 'bpy_app_type' name is only used for examples and messages
                py_descr2predef("", fw, descr, true_module_name, "bpy_app_type", key)
                attribute_set.add(key)

        # Then list the attributes:
        for key, descr in descr_items:
            # naughty, we also add getset's into PyStructs, this is not typical py but also not incorrect.
            if type(descr) == types.MemberDescriptorType:  # 'bpy_app_type' name is only used for examples and messages
                py_descr2predef("", fw, descr, true_module_name, "", key)
                attribute_set.add(key)

        del key, descr

    all_attributes = []
    for attribute in dir(module):
        if (
                (not attribute.startswith("_") and attribute not in submodules)
                and (attribute not in attribute_set)
                and (not attribute.startswith("n_"))
        ):
            all_attributes.append((attribute, getattr(module, attribute)))

    all_attributes = sorted(all_attributes)

    # has_bpy_types = relative_name != 'bpy' and any(isinstance(v, bpy.types.bpy_struct) for _, v in all_attributes)

    #list classes:
    classes = []

    for attribute, value in all_attributes:

        if isinstance(value, (types.FunctionType, types.MethodType)):
            pyfunc2predef("", fw, attribute, value)

        elif isinstance(value, bpy.types.bpy_func) and is_fake_module and isinstance(module, bpy.types.bpy_struct):
            # It should be possible to get the function info
            _structs, funcs, _ops, _props = get_rna_info()
            key = (module.bl_rna.name, attribute)
            if key in funcs:
                rna_function2predef("", fw, funcs[key])
            else:
                print("Could not find rna_function with key: {}".format(key))
                py_c_func2predef("", fw, true_module_name, module, attribute, value, is_class=False)

        # BuiltinMethodType and BuiltinFunctionType both the same at the moment but to be future proof
        elif isinstance(value, (types.BuiltinMethodType, types.BuiltinFunctionType, BMeshOpFuncType, bpy.types.bpy_func)):
            # note: can't get args from these, so dump the string as is
            # this means any module used like this must have fully formatted docstrings.
            py_c_func2predef("", fw, true_module_name, module, attribute, value, is_class=False)

        elif isinstance(value, types.MemberDescriptorType):
            py_descr2predef("", fw, value, true_module_name, "", attribute)

        elif isinstance(value,type):
            classes.append((attribute, value))

        elif isinstance(value, (bool, int, float, str, bytes)):
            # constant, not much fun we can do here except to list it.
            # TODO, figure out some way to document these!
            fw("{0} = {1} # instance value \n\n".format(attribute, repr(value)))

        elif isinstance(value, tuple):
            # tuples are likely to contain useful, constant data, so we'll try to print them
            # Helper function to check if an element of a tuple is printable
            def is_printable(element, recurse=True):
                # For lists, we'll omit their elements
                if isinstance(element, (bool, int, float, str, bytes, NoneType, list)):
                    return True
                elif recurse and isinstance(element, tuple):
                    # For now, we'll only recurse once
                    return all(is_printable(e, recurse=False) for e in element)
                elif element is bpy.app.handlers.persistent:
                    return True
                else:
                    return False

            # Helper function to convert a tuple element to a printable value
            def to_printable(element, recurse=True):
                if isinstance(element, (bool, int, float, str, bytes, NoneType)):
                    return repr(element) if recurse else element
                elif isinstance(element, list):
                    return repr([]) if recurse else []
                elif isinstance(element, tuple):
                    # Currently, we only recurse a maximum of once
                    assert recurse
                    return repr(tuple(to_printable(e, recurse=False) for e in element))
                elif element is bpy.app.handlers.persistent and relative_name == 'bpy.app':
                    # Specifically handle bpy.app.handlers.persistent so that we can print bpy.app.handlers
                    # _persistent is a fake class that we have added to bpy.app
                    return "_persistent"
                else:
                    raise TypeError(f"Invalid type {type(element)}")
            if all(map(is_printable, value)):
                if type(value) != tuple:
                    printed_tuple_subtype = False
                    tuple_subtype = type(value)
                    # Blender uses some c-defined 'named tuples' called PyStructSequences which we can represent as
                    # actual namedtuple instances
                    if (
                            hasattr(tuple_subtype, 'n_sequence_fields')
                            and hasattr(tuple_subtype, 'n_fields')
                            and hasattr(tuple_subtype, 'n_unnamed_fields')
                            and tuple_subtype.n_unnamed_fields == 0  # Not supporting a mix of named and unnamed fields
                    ):
                        # Blender uses some c-defined tuples (StructSequence) that are similar to namedtuples, attempt
                        # to reformat it as a namedtuple
                        # Find all attributes of the class that are MemberDescriptors, these appear to be in the correct
                        # order, but we'll check anyway
                        descriptor_names = []
                        for tuple_attribute_name, tuple_attribute in tuple_subtype.__dict__.items():
                            if isinstance(tuple_attribute, types.MemberDescriptorType):
                                descriptor_names.append(tuple_attribute_name)
                        # Initial check that the length of the tuple matches the length of the descriptor names
                        if len(value) == len(descriptor_names):
                            tuple_values_from_descriptor = []
                            for tuple_attribute_name in descriptor_names:
                                tuple_values_from_descriptor.append(getattr(value, tuple_attribute_name))
                            # Check that the values from descriptors are the same as those from converting to a tuple
                            if tuple(tuple_values_from_descriptor) == tuple(value):
                                # We're good to
                                full_subclass_name = f"{tuple_subtype.__module__}.{tuple_subtype.__name__}"
                                class_args = ', '.join(descriptor_names)
                                instance_args = ', '.join(to_printable(v) for v in value)
                                fw(
                                    "# Actual type is a C defined PyStructSequence. Converted to a namedtuple by pypredef_gen\n"
                                    f"{attribute} = namedtuple('{full_subclass_name}', '{class_args}')({instance_args})\n\n"
                                )
                                printed_tuple_subtype = True
                    # Supporting real namedtuples would be fairly similar
                    # elif (
                    #         hasattr(tuple_subtype, '_asdict')
                    #         and hasattr(tuple_subtype, '_fields')
                    #         and tuple_subtype.__bases__ == (tuple,)
                    # ):
                    #     # Most likely a namedtuple class
                    #     namedtuple_name = tuple_subtype.__name__
                    #     class_args = ', '.join(tuple_subtype._fields)
                    #     instance_args = ', '.join(repr(v) for v in value)
                    #     fw(
                    #         "from collections import namedtuple\n"
                    #         "# Actual type is likely a namedtuple"
                    #         f"{attribute} = namedtuple('{namedtuple_name}', '{class_args}')({instance_args})\n"
                    #         "del namedtuple\n\n"
                    #     )
                    #     printed_tuple_subtype = True
                    if not printed_tuple_subtype:
                        # Other tuple subclasses may not be printable as-is, so create a regular tuple from it and print
                        # that instead
                        fw("{0} = {1} # instance value: {2} \n\n".format(attribute, to_printable(tuple(value)), repr(value)))
                else:
                    fw("{0} = {1} # instance value \n\n".format(attribute, to_printable(value)))
            else:
                # It's difficult to reliably print other types
                elements_text = " (elements omitted)" if value else ""
                fw("{0} = {1}() # instance value{2} \n\n".format(attribute, type(value).__qualname__, elements_text))

        elif isinstance(value, list):
            elements_text = " (elements omitted)" if value else ""
            fw("{0} = [] # instance value{1} \n\n".format(attribute, elements_text))

        elif isinstance(value, set):
            elements_text = " (elements omitted)" if value else ""
            fw("{0} = set() # instance value{1} \n\n".format(attribute, elements_text))

        elif isinstance(value, dict):
            elements_text = " (elements omitted)" if value else ""
            fw("{0} = {{}} # instance value{1} \n\n".format(attribute, elements_text))

        elif isinstance(value, types.MappingProxyType):
            elements_text = " (elements omitted)" if value else ""
            fw("{0} = types.MappingProxyType() # instance value{1} \n\n".format(attribute, elements_text))

        elif isinstance(value, bpy.types.bpy_struct):
            property2predef("", fw, module, attribute)
            # bpy specific
            # # the data members:
            # property2predef("", fw, bpy, "context")
            #
            # property2predef("", fw, bpy, "data")
        else:
            if (
                    value is None
                    and true_module_name == 'bgl'
                    and 'bgl' in _EXTERNAL_MODULE_FUNCTION_RST
                    and attribute in _EXTERNAL_MODULE_FUNCTION_RST[true_module_name]
            ):
                # Many of the bgl functions are typically not loaded, meaning their value is None.
                # We can still document them if we have the external rst though
                py_c_func2predef("", fw, true_module_name, module, attribute, value, is_class=False)
            elif true_module_name in DOCUMENT_ALL_ATTRIBUTES_MODULES:
                # Probably more we can do, but this should suffice for now
                if value is None:
                    if true_module_name == 'bgl':
                        comment = "# function not loaded"
                    else:
                        comment = "# instance value"
                    fw("{} = None {}\n\n".format(attribute, comment))
                else:
                    fw("{} = None # instance value currently of type {}\n\n".format(attribute, type(value)))
            elif not isinstance(value, types.ModuleType):
                print("\tnot documenting %s.%s with type: %s" % (module_name, attribute, type(value)))
                continue

        attribute_set.add(attribute)
        # TODO, more types...

    # write collected classes now
    for (type_name, value) in classes:
        pyclass2predef(fw, true_module_name, type_name, value)

    file.close()

def rna_property2predef(ident, fw, descr):
    ''' Creates declaration of a property
        Details:
        @ident (string): the required prefix (spaces)
        @fw (function): the unified shortcut to print() or file.write() function
        @descr (rna_info.InfoPropertyRNA): descriptor of the property
    '''
    definition = doc2definition(rna2list(descr),docstring_ident="")
    write_indented_lines(ident,fw, definition["declaration"],False)

    if "docstring" in definition:
        write_indented_lines(ident, fw, definition["docstring"], False)

def rna_function2predef(ident, fw, descr, is_bpy_op=False):
    ''' Creates declaration of a function or operator
        Details:
        @ident (string): the required prefix (spaces)
        @fw (function): the unified shortcut to print() or file.write() function
        @descr (rna_info.InfoFunctionRNA or rna_info.InfoOperatorRNA): method's descriptor
    '''
    definition = rna2list(descr)
    if is_bpy_op:
        # bpy.op functions don't report a return type, but as per the documentation, they should return a set of only
        # these values
        bpy_op_return = "set[Literal['RUNNING_MODAL', 'CANCELLED', 'FINISHED', 'PASS_THROUGH', 'INTERFACE']]"
        return_description = {
            "name": "returns",
            "description": "\nRUNNING_MODAL Running Modal – Keep the operator running with blender.\n"
                           "CANCELLED Cancelled – The operator exited without doing anything, so no undo entry should be pushed.\n"
                           "FINISHED Finished – The operator exited after completing its action.\n"
                           "PASS_THROUGH Pass Through – Do nothing and pass the event on.\n"
                           "INTERFACE Interface – Handled but not executed (popup menus).",
            "type": bpy_op_return,
            "ord": len(definition) + 1,  # Not sure what this is, but the other functions for getting the return had it
        }
        definition.setdefault("@returns", return_description)

    definition = doc2definition(definition)

    declaration = definition["declaration"]
    if is_bpy_op:
        # bpy.op functions take 3 optional positional arguments, in the form of
        # override_context: dict, execution_context: str, undo: bool
        # but these cannot be specified by name, so we must present them as *args
        # Insert the *args parameter into the declaration
        no_existing_args = declaration.find('()') != -1
        declaration = declaration.replace('(', "(*args: Union[dict, str, bool]" if no_existing_args else "(*args: Union[dict, str, bool], ", 1)

    write_indented_lines(ident,fw,declaration,False) #may contain two lines: decorator and declaration

    if "docstring" in definition:
        write_indented_lines(ident, fw, definition["docstring"], False)

    if "returns" in definition:
        # The return type is hinted by the declaration. Since this is a stub, return ...
        write_indented_lines(ident+_IDENT, fw, "return ...", False)
    else:
        write_indented_lines(ident+_IDENT, fw, "pass", False)

    fw("\n")


def rna_struct2predef(ident, fw, descr: rna_info.InfoStructRNA, is_fake_module=False, module_name=None):
    ''' Creates declaration of a bpy structure
        Details:
        @ident (string): the required prefix (spaces)
        @fw (function): the unified shortcut to print() or file.write() function
        @descr (rna_info.InfoStructRNA): the descriptor of a Blender Python class
        @fake_module (bool): if true, print only the properties and functions without indentation and return attribute names
    '''

    if not is_fake_module:
        print("class %s:" % descr.identifier)
        definition = doc2definition(rna2list(descr), module_name=module_name)
        write_indented_lines(ident,fw, definition["declaration"],False)

        if "docstring" in definition:
            write_indented_lines(ident, fw, definition["docstring"], False)

        #native properties
        ident = ident + _IDENT

    rna_properties = descr.properties
    rna_properties.sort(key= lambda prop: prop.identifier)
    for prop in rna_properties:
        rna_property2predef(ident,fw,prop)

    #Python properties
    py_properties = descr.get_py_properties()
    for identifier, prop in py_properties:
        pyprop2predef(ident,fw,identifier,prop)

    #Blender native functions
    rna_functions = descr.functions
    for function in rna_functions:
        rna_function2predef(ident, fw, function)

    py_functions = descr.get_py_functions()
    for identifier, function in py_functions:
        pyfunc2predef(ident, fw, identifier, function, attribute_defined_class=descr.py_class)

    if is_fake_module:
        all_attributes = set()
        all_attributes.update(prop.identifier for prop in rna_properties)
        all_attributes.update(func.identifier for func in rna_functions)
        all_attributes.update(prop[0] for prop in py_properties)
        all_attributes.update(func[0] for func in py_functions)
        return all_attributes

def ops_struct2predef(ident, fw, module, operators):
    ''' Creates "pseudostructure" for a given module of operators
        Details:
        @ident (string): the required prefix (spaces)
        @fw (function): the unified shortcut to print() or file.write() function
        @module (string): one of bpy.ops names ("actions", for example)
        @operators (list of rna_info.InfoOperatorRNA): operators, grouped in this module
    '''
    fw("from typing import Literal, Union\n\n")
    # fmt = ident + "class {0}:\n"
    # fw(fmt.format(module)) #"action" -> "class action:\n"
    # ident = ident+_IDENT
    # fw(ident+"'''Special class, created just to reflect content of bpy.ops.{0}'''\n\n".format(module))

    operators.sort(key=lambda op: op.func_name)

    for operator in operators:
        rna_function2predef(ident, fw, operator, is_bpy_op=True)

def bpy_base2predef(ident, fw):
    ''' Creates a structure for the Blender base class
        Details:
        @ident (string): the required prefix (spaces)
        @fw (function): the unified shortcut to print() or file.write() function\n
    '''

    def print_base_class(ident, base_class, available_in_blender=True):
        class_name = base_class.__name__
        # Some classes exist, but aren't available, so we mark them as private by prefixing them with '_' and adding a
        # mapping to TYPE_ABERRATIONS
        modified_class_name = TYPE_ABERRATIONS.get(base_class.__name__, base_class.__name__)
        non_object_bases = (base.__name__ for base in base_class.__bases__ if base != object)
        non_object_bases = [TYPE_ABERRATIONS.get(name, name) for name in non_object_bases]
        if non_object_bases:
            fmt = ident + "class %s(%s):\n"
            fw(fmt % (modified_class_name, ", ".join(non_object_bases)))
        else:
            fmt = ident + "class %s:\n"
            fw(fmt % modified_class_name)
        ident = ident + _IDENT
        fw(ident + "'''built-in base class for classes in bpy.types.\n\n")
        if not available_in_blender:
            fmt = ident + _IDENT + "Note that bpy.types.%s is not actually available from within blender, it only exists for the purpose of documentation.\n" + ident + "'''\n\n"
            fw(fmt % class_name)
        else:
            fw(ident + _IDENT + "\n" + ident + "'''\n\n")

        descr_items = [(key, descr) for key, descr in sorted(base_class.__dict__.items()) if key not in IGNORED_CLASS_KEYS]

        for key, descr in descr_items:
            # Write all the extra __<name>__ functions that are defined on this class
            if isinstance(descr, types.WrapperDescriptorType):
                pyfunc2predef(ident, fw, key, descr, attribute_defined_class=base_class)

        for key, descr in descr_items:
            if type(descr) == types.MethodDescriptorType:
                py_descr2predef(ident, fw, descr, "bpy.types", modified_class_name, key)

        for key, descr in descr_items:
            if type(descr) == types.GetSetDescriptorType:
                py_descr2predef(ident, fw, descr, "bpy.types", modified_class_name, key)

        fw("\n\n")

    available_base_classes = [
        bpy.types.bpy_struct,
        bpy.types.bpy_struct_meta_idprop,
        bpy.types.bpy_func,
        # Type for most user defined properties, adding is as a return type hint for bpy.props could be helpful
        bpy.types.bpy_prop,
        # Mesh.vertices, for example, doesn't actually return MeshVertices, it actually returns a
        # bpy.types.bpy_prop_collection whose .bl_rna returns <bpy_struct, Struct("MeshVertices") at [address]>
        bpy.types.bpy_prop_collection,
        # Image.pixels for example
        bpy.types.bpy_prop_array,
    ]

    not_available_base_classes = []

    # A CollectionProperty added to an ID type by an addon uses this type
    collection_idprop_name = 'bpy_prop_collection_idprop'
    # Currently it is not available in bpy.types, but it could be in the future
    bpy_prop_collection_idprop_available = hasattr(bpy.types, collection_idprop_name)
    if bpy_prop_collection_idprop_available:
        # The type is available
        available_base_classes.append(bpy.types.bpy_prop_collection_idprop)
    else:
        # The type is not available, get it from bpy_prop_collection's subclasses and prefix its name with '_'
        prop_collection_subclasses = bpy.types.bpy_prop_collection.__subclasses__()
        bpy_prop_collection_idprop = next(
            (subclass for subclass in prop_collection_subclasses if subclass.__name__ == collection_idprop_name),
            None
        )
        if bpy_prop_collection_idprop is not None:
            # Add a mapping into the TYPE_ABERRATIONS to the prefixed name in-case something else uses this type
            TYPE_ABERRATIONS[collection_idprop_name] = "_bpy_prop_collection_idprop"
            not_available_base_classes.append(bpy_prop_collection_idprop)

    for base_class in available_base_classes:
        print_base_class(ident, base_class)

    for base_class in not_available_base_classes:
        print_base_class(ident, base_class, available_in_blender=False)

def property2predef(ident, fw, module, name):
    ''' writes definition of a named property
        Details:
        @ident (string): the required prefix (spaces)
        @fw (function): the unified shortcut to print() or file.write() function
        @module (string): one of bpy.ops names ("actions", for example)
        @name (string): name of the property
    '''
    value = getattr(module, name, None)
    if value:
        value_type = getattr(value, "rna_type", None)
        module_name = module.__name__ if isinstance(module, types.ModuleType) else module
        if module_name == 'bpy':
            prefix = "types."
        elif module_name == 'bpy.types':
            prefix = ""
        else:
            prefix = "bpy.types."
        if value_type:
            fw("{0} = {1}{2}\n".format(name, prefix, value_type.identifier))
        else:
            pyclass2predef(fw, module, name, value)
    fw("\n\n")


_RNA_STRUCTS = None
_RNA_FUNCS = None
_RNA_OPS = None
_RNA_PROPS = None


def get_rna_info():
    global _RNA_STRUCTS, _RNA_FUNCS, _RNA_OPS, _RNA_PROPS
    if _RNA_STRUCTS is None or _RNA_FUNCS is None or _RNA_OPS is None or _RNA_PROPS is None:
        # Note that by default, rna_info filters out some classes, such as most Operators, this can be changed by
        # monkey-patching rna_info.rna_id_ignore before calling BuildRNAInfo()
        old_func = rna_info.rna_id_ignore
        rna_info.rna_id_ignore = lambda rna_id: rna_id == "rna_type" or "_OT_" in rna_id
        info = rna_info.BuildRNAInfo()
        rna_info.rna_id_ignore = old_func
        _RNA_STRUCTS, _RNA_FUNCS, _RNA_OPS, _RNA_PROPS = info
        return info
    return _RNA_STRUCTS, _RNA_FUNCS, _RNA_OPS, _RNA_PROPS


def bpy2predef(BASEPATH, title, write_ops, write_types):
    ''' Creates the bpy.predef file. It contains the bpy.dta, bpy.ops, bpy.types
        Arguments:
        BASEPATH (string): path for the output file
        title(string): descriptive title (the comment for the whole module)
    '''
    #read all data:
    structs, funcs, ops, props = get_rna_info()

    # make bpy directory
    dirpath = os.path.join(BASEPATH, "bpy")
    if not os.path.isdir(dirpath):
        os.mkdir(dirpath)

    # # open the file:
    # filepath = os.path.join(dirpath, "__init__.py")
    # file = open(filepath, "w")
    # fw = file.write
    # #Start the file:
    # definition = doc2definition(title, "")  # skip the leading spaces at the first line...
    # fw(definition["docstring"])
    # fw("\n\n")
    #
    #
    # file.close()

    if write_ops:
        #group operators by modules: (dictionary of list of the operators)
        ops_dirpath = os.path.join(dirpath, "ops")
        if not os.path.isdir(ops_dirpath):
            os.mkdir(ops_dirpath)
        op_modules = {}
        for op in ops.values():
            op_modules.setdefault(op.module_name, []).append(op)

        sorted_op_modules = sorted(op_modules.items(), key=lambda m: m[0])

        #Special declaration of non-existing structiure just fo the ops member:
        ops_init_filepath = os.path.join(ops_dirpath, "__init__.py")
        file = open(ops_init_filepath, "w")
        fw = file.write
        fw("'''bpy.ops submodule'''\n\n")
        for op_module_name, _ in sorted_op_modules:
            fw("from . import " + op_module_name + "\n")
        file.close()

        for op_module_name, ops_mod in sorted_op_modules:
            if op_module_name == "import":
                continue
            # Write to {op_module_name}.py file in the ops directory
            ops_module_filepath = os.path.join(ops_dirpath, "{}.py".format(op_module_name))
            file = open(ops_module_filepath, "w")
            fw = file.write
            ops_struct2predef("", fw, op_module_name, ops_mod)
            file.close()

    if write_types:
        #classes (Blender structures:)
        types_dirpath = os.path.join(dirpath, "types")
        if not os.path.isdir(types_dirpath):
            os.mkdir(types_dirpath)
        types_init_filepath = os.path.join(types_dirpath, "__init__.py")
        file = open(types_init_filepath, "w")
        # file = open(os.path.join(dirpath, "types.py"), "w")
        fw = file.write
        fw("'''bpy.types submodule'''\n\n")

        fw("from ... import bpy\n"
           "from ... import mathutils\n")
        # Extra imports for type hints
        fw("from typing import Union, Literal, Annotated, Sequence, TypeVar, Iterator, Optional, Generic, overload\n\n")

        #base structure
        bpy_base2predef("", fw)

        # Some bpy_prop_collections have int keys and some have str keys, I don't know if there's a way to tell them
        # apart
        # Note that only the collections in bpy.data support get(key: tuple[str, Optional[str]])
        bpy_prop_collection_idprop_name = TYPE_ABERRATIONS.get("bpy_prop_collection_idprop", "bpy_prop_collection_idprop")
        fake_generic_classes = (
            f"""
# Prop collection keys can be either int or str
_prop_collection_key = Union[int, str]
_T = TypeVar('_T')
_R = TypeVar('_R', bounds='type')

class _generic_prop(Generic[_R], bpy_prop):
    \"\"\"Fake generic version of bpy_prop added by pypredef_gen\"\"\"
    rna_type: _R
    data: bpy.types.bpy_struct
    id_data: Optional[bpy.types.ID]

class _generic_prop_collection(Generic[_T], bpy_prop_collection):
    \"\"\"Fake generic version of bpy_prop_collection added by pypredef_gen\"\"\"
    @overload
    def __getitem__(self, key: _prop_collection_key) -> _T: ...
    @overload
    def __getitem__(self, s: slice) -> Sequence[_T]: ...
    def __iter__(self) -> Iterator[_T]: ...
    def find(self, key: str) -> int: ...
    def get(self, key: Union[str, tuple[str, Optional[str]]], default=None) -> _T: ...
    def items(self) -> list[tuple[_prop_collection_key, _T]]: ...
    def keys(self) -> list[str]: ...
    def values(self) -> list[_T]: ...

class _generic_prop_array(Generic[_T], bpy_prop_array):
    \"\"\"Fake generic version of bpy_prop_array added by pypredef_gen\"\"\"
    @overload
    def __getitem__(self, key: int) -> _T: ...
    @overload
    def __getitem__(self, s: slice) -> Sequence[_T]: ...
    def __getitem__(self, key) -> _T: ...
    def __iter__(self) -> Iterator[_T]: ...

class _generic_prop_collection_idprop(_generic_prop_collection[_T], {bpy_prop_collection_idprop_name}):
    \"\"\"Fake generic version of bpy_prop_collection_idprop (not available from within Blender) added by pypredef_gen\"\"\"
    pass

""")
        fw(fake_generic_classes)

        #sort the type names:
        classes = list(structs.values())
        classes.sort(key=lambda cls: cls.identifier)

        def get_top_level_base(struct) -> str:
            base = struct.base
            while base:
                base_base = base.base
                if base_base:
                    base = base_base
                else:
                    return base.identifier
            return "bpy_struct"

        structs_by_base = defaultdict(list)
        for struct in classes:
            structs_by_base[get_top_level_base(struct)].append(struct)

        for base_name, structs_list in structs_by_base.items():
            base_module_name = "_types_" + base_name.lower()
            fw(f"from .{base_module_name} import {', '.join(s.identifier for s in structs_list)}\n")

            base_module_filepath = os.path.join(types_dirpath, f"{base_module_name}.py")
            base_module_file = open(base_module_filepath, "w")
            bm_fw = base_module_file.write
            bm_fw(f'"""fake bpy.types submodule for {base_name} types"""\n\n')

            bm_fw("from ... import bpy\n"
               "from ... import mathutils\n")
            # Extra imports for type hints
            bm_fw("from typing import Union, Literal, Annotated, Sequence, TypeVar, Iterator, Optional, Generic, overload\n\n")

            for struct in structs_list:
                rna_struct2predef("", bm_fw, struct, module_name="bpy.types")

            base_module_file.close()

        file.close()

def rna2predef(BASEPATH):

    try:
        os.mkdir(BASEPATH)
    except:
        pass

    # Special handling for these modules
    included_special_modules = _SPECIAL_HANDLING_MODULES.intersection(INCLUDE_MODULES)

    if included_special_modules or 'bpy' in INCLUDE_MODULES:
        write_ops = 'bpy.ops' not in EXCLUDE_MODULES
        write_types = 'bpy.types' not in EXCLUDE_MODULES
        bpy2predef(BASEPATH, "Blender API main module", write_ops, write_types)

    # None of the special modules should be included if found recursively from other modules, so they are always
    # excluded. For example, bpy can be found at bpy_extras.asset_utils.bpy
    EXCLUDE_MODULES.update(_SPECIAL_HANDLING_MODULES)
    # internal modules

    module = None

    # Some modules may reference other modules in a loop so a set is needed to keep track of which modules have
    # already been visited
    visited = set()

    if 'freestyle' in INCLUDE_MODULES and '_freestyle' not in EXCLUDE_MODULES:
        # The modules in freestyle import from the private, C-defined module _freestyle, include it if freestyle is
        # included
        INCLUDE_MODULES.append("_freestyle")

    for module_name in INCLUDE_MODULES:
        if module_name not in EXCLUDE_MODULES:
            module = importlib.import_module(module_name)
            if module_name == 'bgl':
                # reloading bgl seems to get more of the functions to load
                module = importlib.reload(module)
            print("\n\n--- Generating predef for included module {}".format(module_name))
            pymodule2predef(BASEPATH, module_name, module, "Module {}".format(module_name), visited)

    del module


def load_external_rst(external_rst_dir):
    external_files = os.listdir(external_rst_dir)
    for f in external_files:
        if not f.endswith(".rst"):
            print("Found file not ending in .rst: {}. Skipping.".format(f))

        file = open(os.path.join(external_rst_dir, f), 'r')
        contents = file.read()
        module_name, module_rst, function_rst_dict, class_rst_dict = rst_module_to_rst_dict(contents)

        function_rst_dict = {function_name: rst2list(function_rst) for function_name, function_rst in function_rst_dict.items()}
        # Parsing classes needs some work as currently the bgl.Buffer class copies its entire contents
        # as the docstring, albeit with words and bits of its formatting cut out.
        class_rst_dict = {class_name: rst2list(class_rst) for class_name, class_rst in class_rst_dict.items()}

        _EXTERNAL_MODULE_FUNCTION_RST[module_name] = function_rst_dict
        _EXTERNAL_MODULE_CLASS_RST[module_name] = class_rst_dict
        file.close()


def main():
    global _ARG_SKIP_FILES
    import bpy
    if 'bpy' not in dir():
        print("\nError, this script must run from inside blender")
        print(script_help_msg)
    else:
        argv = sys.argv
        # Get the args passed to blender after "--", all of which are ignored by blender so that scripts can receive
        # their own arguments
        if "--" not in argv:
            argv = []  # as if no args are passed
        else:
            argv = argv[argv.index("--") + 1:]  # get all args after "--"

        parser = argparse.ArgumentParser()

        parser.add_argument("--external-rst-dir", dest="external_rst_dir", type=str, required=False,
                            help="Directory containing external .rst files to be loaded")

        parser.add_argument("--skip-files", "-s", dest="skip_files", action='store_true', default=False, required=False,
                            help="Skip modules that already exist as files instead of copying the files")

        args = parser.parse_args(argv)

        if args.external_rst_dir:
            load_external_rst(args.external_rst_dir)

        _ARG_SKIP_FILES = args.skip_files

        #these two strange lines below are just to make the debugging easier (to let it run many times from within Blender)
        import imp
        imp.reload(rna_info) #to avoid repeated arguments in function definitions on second and the next runs - a bug in rna_info.py....

        if len(bpy.data.filepath)==0 or __file__[:len(bpy.data.filepath)] != bpy.data.filepath:
            script_dir = os.path.dirname(__file__)
        else:
            #program run from text window has wacky filename
            buffer_name = __file__[len(bpy.data.filepath) + 1:]
            buffer = bpy.data.texts[buffer_name]
            script_dir = os.path.dirname(buffer.filepath) #directory where this pypredef_gen.py script lives

        path_in = os.path.join(script_dir, "pypredef")
        # only for partial updates
        path_in_tmp = path_in + "-tmp"

        if not os.path.exists(path_in):
            os.mkdir(path_in)

        # only for full updates
        if _BPY_FULL_REBUILD:
            shutil.rmtree(path_in, True)
        else:
            # write here, then move
            shutil.rmtree(path_in_tmp, True)

        rna2predef(path_in_tmp)

        if not _BPY_FULL_REBUILD:
            import filecmp

            # now move changed files from 'path_in_tmp' --> 'path_in'

            file_list_path_in = set()
            for dirpath, _, filenames in os.walk(path_in):
                for filename in filenames:
                    file_list_path_in.add(os.path.relpath(os.path.join(dirpath, filename), path_in))
            file_list_path_in_tmp = set()
            for dirpath, _, filenames in os.walk(path_in_tmp):
                for filename in filenames:
                    file_list_path_in_tmp.add(os.path.relpath(os.path.join(dirpath, filename), path_in_tmp))

            # remove deprecated files that have been removed.
            for f in sorted(file_list_path_in):
                if f not in file_list_path_in_tmp and f != "__pycache__":
                    print("\tdeprecated: %s" % f)
                    os.remove(os.path.join(path_in, f))

            # freshen with new files.
            for f in sorted(file_list_path_in_tmp):
                f_from = os.path.join(path_in_tmp, f)
                f_to = os.path.join(path_in, f)

                do_copy = True
                if f in file_list_path_in:
                    if filecmp.cmp(f_from, f_to):
                        do_copy = False

                if do_copy:
                    os.makedirs(os.path.dirname(f_to), exist_ok=True)
                    print("\tupdating: %s" % f)
                    shutil.copy(f_from, f_to)
                '''else:
                    print("\tkeeping: %s" % f) # eh, not that useful'''
        if bpy.app.background:
            print("\nSome graphics dependent types and modules (mainly the bgl module) may not be loaded fully when"
                  " running with the -b argument. If you need full predefinition files for graphics related types and"
                  " modules, run the script without the -b argument")



main() #just run it! Unconditional call makes it easier to debug Blender script in Eclipse,
#       (using pydev_debug.py). It's doubtful, that it will be imported as additional module.


if __name__ == '__main__':
    sys.exit() #Close Blender, when you run it from the command line....
