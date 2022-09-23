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
    "bl_operators",
    "blf",
    "mathutils",
    "gpu",
    "gpu_extras",
    "imbuf",
    "freestyle",
    "rna_info",
    "idprop",
    "nodeitems_utils",
]

# Default addon modules:
ADDON_MODULES = [
    "io_anim_bvh",
    "io_scene_fbx",
    "cycles",
    "io_scene_obj",
    "io_mesh_ply",
    "io_mesh_stl",
    "io_scene_x3d",
]

_ADDON_MODULES_INCLUDED = False

# Modules to exclude, each module's .__name__ is checked against this set
EXCLUDE_MODULES = {
    "dummy_module_for_formatting_purposes",
}

# Non-module attributes to treat as if they are modules
ATTRIBUTES_AS_SUBMODULES = {
    "bpy": {
        "app",
    },
}

# Modules/submodules that will include all attributes, initialised to None if the type can't be represented
DOCUMENT_ALL_ATTRIBUTES_MODULES = {
    "bpy",
    "bgl",
}

_ERRORS = []
_WARNINGS = []

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
import builtins
import inspect
import types
import importlib
import bpy
import bpy_types
import mathutils
import rna_info
import bmesh
import argparse
import shutil
import re
from itertools import chain
from typing import Callable, Optional, Union, NamedTuple, Any, Literal, overload
from collections import defaultdict
from dataclasses import dataclass, field
from enum import Enum

# BMeshOpFunc type is not exposed directly by the current Blender API
BMeshOpFuncType = type(bmesh.ops.split)
NoneType = types.NoneType if hasattr(types, 'NoneType') else type(None)


def _find_float_vector_prop_types(id_instance: bpy.types.ID) -> dict[type, dict[tuple[str, ...], list[range]]]:
    """FloatVectorProperties change type based on the subtype and size, this function will try each combination,
    observe the resulting type and then combine
    An instance of ID (technically a Bone or PoseBone would work too) is required so that custom properties can be
    added to its type and then retrieved from the instance. A type with few instances such as bpy.types.Scene is
    recommended"""
    subtypes = (
        'COLOR', 'TRANSLATION', 'DIRECTION', 'VELOCITY', 'ACCELERATION', 'MATRIX', 'EULER', 'QUATERNION', 'AXISANGLE',
        'XYZ', 'XYZ_LENGTH', 'COLOR_GAMMA', 'COORDINATES', 'LAYER', 'LAYER_MEMBER', 'NONE'
    )

    # TODO: In newer blender versions, this can be a Sequence of up to 3 ints for multiple dimensions.
    #  When it's a sequence with more than one element, it looks like it always results in a bpy_prop_array, except when
    #  the Sequence is [3,3] or [4,4] and subtype='MATRIX' and it results in a mathutils.Matrix
    # min length is 1
    # max length is 32
    size_range = range(1, 33)

    id_type = type(id_instance)

    from itertools import groupby, product

    resulting_types_for_each_arg_combination: dict[type, dict[str, list[int]]] = {}

    # Iterate through the cartesian product of the subtypes and sizes and record the resulting type of the property for
    # each combination
    for subtype, size in product(subtypes, size_range):
        id_type.pypredef_prop = bpy.props.FloatVectorProperty(size=size, subtype=subtype)
        result_type = type(id_instance.pypredef_prop)
        # if result_type != bpy.types.bpy_prop_array:
        resulting_types_for_each_arg_combination.setdefault(result_type, {}).setdefault(subtype, []).append(size)

    combinations: dict[type, dict[tuple[str, ...], list[range]]] = {}
    # Iterate through the results and convert the lists of ints into lists of ranges and combine subtypes with the same
    # ranges for each type
    for result_type, result_type_dict in resulting_types_for_each_arg_combination.items():
        result_type_combinations: dict[range, list[str]] = {}
        for subtype, sizes in result_type_dict.items():
            # Convert list of integers into ranges
            for a, b in groupby(enumerate(sizes), lambda pair: pair[1] - pair[0]):
                b = list(b)
                range_from_ints = range(b[0][1], b[-1][1] + 1)
                # Add subtype to list in dict keyed by the range
                result_type_combinations.setdefault(range_from_ints, []).append(subtype)

        final_results: dict[tuple[str, ...], list[range]] = {}
        for r, subtype_list in result_type_combinations.items():
            # Can't use a list as a key, so convert to tuple, we'll also sort to ensure we can't end up with subtypes
            # in different orders
            final_results.setdefault(tuple(sorted(subtype_list)), []).append(r)

        combinations[result_type] = final_results

    # Remove the property
    if hasattr(id_type, 'pypredef_prop'):
        del id_type.pypredef_prop

    return combinations


class FuncTypeEnum(Enum):
    FUNCTION = 0,
    INSTANCE_METHOD = 1,
    CLASS_METHOD = 2,
    STATIC_METHOD = 3,


class FuncType(NamedTuple):
    func_type: FuncTypeEnum
    bind_target: Any = None
    unbound_method: Optional[Union[classmethod, staticmethod]] = None

    @classmethod
    def from_descr(cls, method_func_or_descriptor):
        if isinstance(method_func_or_descriptor, types.MethodDescriptorType):
            return cls(FuncTypeEnum.INSTANCE_METHOD, bind_target=method_func_or_descriptor.__objclass__)
        elif isinstance(method_func_or_descriptor, types.ClassMethodDescriptorType):
            return cls(FuncTypeEnum.CLASS_METHOD, bind_target=method_func_or_descriptor.__objclass__)
        elif isinstance(method_func_or_descriptor, classmethod):
            return cls(FuncTypeEnum.CLASS_METHOD, unbound_method=method_func_or_descriptor)
        elif isinstance(method_func_or_descriptor, staticmethod):
            return cls(FuncTypeEnum.STATIC_METHOD, unbound_method=method_func_or_descriptor)
        else:
            return cls(FuncTypeEnum.FUNCTION)

    def is_function(self):
        return self.func_type == FuncTypeEnum.FUNCTION

    def is_instance_method(self):
        return self.func_type == FuncTypeEnum.INSTANCE_METHOD

    def is_class_method(self):
        return self.func_type == FuncTypeEnum.CLASS_METHOD

    def is_static_method(self):
        return self.func_type == FuncTypeEnum.STATIC_METHOD


def get_top_level_module(module_str: str) -> str:
    """Helper function to get the top-level module from a module string"""
    return module_str.split('.', maxsplit=1)[0]


def is_included(class_or_module: Union[type, types.ModuleType, str]) -> bool:
    """Helper function to check if a class is in or a module is an included module"""
    if isinstance(class_or_module, type):
        module_str = class_or_module.__module__
    elif isinstance(class_or_module, types.ModuleType):
        module_str = class_or_module.__name__
    elif isinstance(class_or_module, str):
        module_str = class_or_module
    else:
        raise TypeError(f"Expected a class, module or string, but got '{class_or_module}'")
    return get_top_level_module(module_str) in INCLUDE_MODULES


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


def get_full_type_name(type_: type, module_override=None) -> str:
    """Helper function to get full name of type, including module"""
    if module_override:
        return module_override + "." + type_.__qualname__

    if type_.__module__ == 'builtins':
        if not hasattr(builtins, type_.__name__):
            # Type comes from C, but isn't in builtins, so we'll have to guess as to where it can be found
            if hasattr(bpy.types, type_.__name__):
                return "bpy.types." + type_.__name__
            elif hasattr(mathutils, type_.__name__):
                return "mathutils." + type_.__name__
            else:
                raise RuntimeError(f"Unknown module for type: {type_}")
        return type_.__qualname__
    else:
        return type_.__module__ + "." + type_.__qualname__


def is_tuple_pystructsequence(value):
    """Return whether a tuple is a PyStructSequence"""
    attributes = ('n_sequence_fields', 'n_fields', 'n_unnamed_fields')
    return type(value) != tuple and all(hasattr(value, a) for a in attributes)


def is_printable_tuple_element(element, recurse=True):
    """Helper function to check if an element of a tuple is printable"""
    # For lists, we'll omit their elements
    if isinstance(element, (bool, int, float, str, bytes, NoneType, list)):
        return True
    elif recurse and isinstance(element, tuple):
        # For now, we'll only recurse once
        return all(is_printable_tuple_element(e, recurse=False) for e in element)
    elif element is bpy.app.handlers.persistent:
        return True
    else:
        return False


def printable_tuple_repr(element, recurse=True):
    """Helper function to convert a tuple or tuple element to a printable value, only 1 level of recursion for nested
     tuples"""
    if isinstance(element, (bool, int, float, str, bytes, NoneType)):
        return repr(element) if recurse else element
    elif isinstance(element, list):
        # Lists are usually not fixed values, so we don't print their contents
        return repr([]) if recurse else []
    elif isinstance(element, tuple):
        # Currently, we only recurse a maximum of once
        assert recurse
        return repr(tuple(printable_tuple_repr(e, recurse=False) for e in element))
    elif element is bpy.app.handlers.persistent:
        # Specifically handle bpy.app.handlers.persistent so that we can print bpy.app.handlers
        # _persistent is a fake class that we have added to bpy.app
        return "_persistent"
    else:
        raise TypeError(f"Invalid type {type(element)}")


def print_pystructsequence(ident, fw, attribute_name, value):
    # Can only represent as a namedtuple if all the fields are named (all the ones Blender uses appear to have all
    # named fields)
    if value.n_unnamed_fields == 0:
        pystructsequence_type = type(value)
        # Find all attributes of the class that are MemberDescriptors, these appear to be in the correct
        # order, but we'll check anyway
        descriptor_names = []
        for tuple_attribute_name, tuple_attribute in pystructsequence_type.__dict__.items():
            if isinstance(tuple_attribute, types.MemberDescriptorType):
                descriptor_names.append(tuple_attribute_name)
        # Check that the length of the tuple matches the length of the descriptor names
        if len(value) == len(descriptor_names):
            tuple_values_from_descriptor = []
            for tuple_attribute_name in descriptor_names:
                tuple_values_from_descriptor.append(getattr(value, tuple_attribute_name))
            # Check that the values from descriptors are the same as those from converting to a tuple
            if tuple(tuple_values_from_descriptor) == tuple(value):
                # We're good to
                full_subclass_name = f"{pystructsequence_type.__module__}.{pystructsequence_type.__name__}"
                class_args = ', '.join(descriptor_names)
                instance_args = ', '.join(printable_tuple_repr(v) for v in value)
                fw(
                    f"{ident}# Actual type is a C defined PyStructSequence. Converted to a namedtuple by pypredef_gen\n"
                    f"{ident}{attribute_name} = namedtuple('{full_subclass_name}', '{class_args}')({instance_args})\n\n"
                )
                return
    # If we reached here there was a problem with representing the PyStructSequence as a namedtuple
    # We'll convert to a standard tuple and print that instead, the repr of the PyStructSequence will go in a comment
    fw(f"{ident}{attribute_name} = {printable_tuple_repr(tuple(value))} # instance value: {repr(value)} \n\n")


@dataclass
class Line:
    """Base class for a line (or multiline) of parsed rst"""
    description: str = None


@dataclass
class DefLine(Line):
    """Represents a parsed rst declaration"""
    prototype: str = None
    return_type: Optional[str] = None
    is_fake_class: bool = False
    bases_fake: bool = False
    hint: str = None
    decorator: str = None
    function_type: str = None


@dataclass
class ArgumentLine(Line):
    """Represents a parsed rst argument"""
    name: str = None
    type: str = None


@dataclass
class ReturnsLine(ArgumentLine):
    """Represents a parsed rst return type"""
    name: str = "returns"


@dataclass
class NoteLine(Line):
    """Represents a parsed rst note"""
    pass


@dataclass
class SeeAlsoLine(Line):
    """Represents a parsed rst seealso"""
    pass


@dataclass
class DefinitionParts:
    """Represents a parsed rst"""
    definition_def: DefLine = field(default_factory=DefLine)
    argument_defs: dict[str, ArgumentLine] = field(default_factory=dict)
    returns_def: Optional[ReturnsLine] = None
    note_def: Optional[NoteLine] = None
    see_also_def: Optional[SeeAlsoLine] = None


class Definition(NamedTuple):
    """Parsed rst, formatted into separate printable parts"""
    docstring: str
    declaration: str = None
    returns: str = None
    returns_comment: str = None
    function_type: str = None


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
def rst2list(doc: str):
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
                 ["@returns":]
                         optional: what function/property returns:
                         "description": description of the content
                         "type":        the name of returned type
                 ["@note":]
                         optional: note, added to description (below argument list)
                         "description": description of the content
                 ["@seealso":]
                         optional: reference, added to description (below argument list)
                         "description": description of the content
    '''
    def process_line(line: str, definition: DefinitionParts, last_entry: Line):
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
            def_line = definition.definition_def
            last_entry = def_line
            if def_line.prototype is None:
                def_line.prototype = prototype
            if def_line.function_type is None:
                def_line.function_type = function_type
        elif line.startswith(":arg"):
            expr = line.split(" ",2)
            name = expr[1].rstrip(":")
            if len(expr) == 3:
                arg_line = ArgumentLine(name=name, description=expr[2])
            else:
                arg_line = ArgumentLine(name=name, description="")
            definition.argument_defs.setdefault(name, arg_line)
            last_entry = arg_line
        elif line.startswith(":type:"): #property type
            expr = type_name(line)
            def_line = definition.definition_def
            if expr and def_line.return_type is None:
                def_line.return_type = expr
            last_entry = def_line
        elif line.startswith(":return:"): #return description
            if definition.returns_def is None:
                expr = line.split(" ", 1)
                returns = ReturnsLine(description=expr[1])
                definition.returns_def = returns
                last_entry = returns
        elif line.startswith(":rtype:"): #type, returned by the function
            if isinstance(last_entry, ReturnsLine):
                expr = type_name(line)
                if expr:
                    last_entry.type = expr
            else:
                def_line = definition.definition_def
                last_entry = def_line
                if def_line.return_type is None:
                    expr = type_name(line)
                    if expr:
                        def_line.return_type = expr
        elif line.startswith(":type"): #argument's type
            expr = line.split(" ",2)
            name = expr[1].rstrip(":")
            try:
                arg_line = definition.argument_defs[name]
                if arg_line.type is None:
                    arg_line.type = expr[2]
                last_entry = arg_line
            except:
                print("Missing argument declaration for '%s'" % name)
        elif line.startswith(".. note:: "): #note to member description
            line = line.replace(".. note:: ","")
            last_entry = definition.note_def
            if not last_entry:
                last_entry = NoteLine(description=line)
                definition.note_def = last_entry
        elif line.startswith(".. seealso::"): #reference to external resource
            line = line.replace(".. seealso:: ","")
            last_entry = definition.see_also_def
            if not last_entry:
                last_entry = SeeAlsoLine(description=line)
                definition.see_also_def = last_entry
        elif line.startswith(".. literalinclude::"):
            pass #skip this line
        else: #this is just second line of description for the last entry
            #  (whole member, or just an single argument)
            if line != "" and not line.startswith("Undocumented"):
                if last_entry.description is None:
                    last_entry.description = line
                else:
                    last_entry.description = last_entry.description + line + "\n"
        return last_entry
    #--------------------------------- process_line
    lines = doc.split("\n")
    last_key = DefLine(description="")
    definition = DefinitionParts(definition_def=last_key)  # at the beginning: empty description of function definition

    for line in lines:
        last_key = process_line(line,definition,last_key)

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

def rna2list(info, extra_property_types=()) -> DefinitionParts:
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

    @overload
    def get_argitem(arg: rna_info.InfoPropertyRNA, is_return: Literal[True]) -> ReturnsLine: ...
    @overload
    def get_argitem(arg: rna_info.InfoPropertyRNA, is_return: Literal[False] = False) -> ArgumentLine: ...

    def get_argitem(arg: rna_info.InfoPropertyRNA, is_return: bool = False) -> ArgumentLine:
        '''Helper function, that creates an argument definition subdictionary
           Arguments:
           @arg (rna_info.InfoPropertyRNA): descriptor of the argument

           Returns: an definistion subdictionary (keys: "name", "type", "description")
        '''
        if arg.fixed_type:
            arg_type = arg.fixed_type.identifier
        else:
            arg_type = arg.type
        if arg_type == 'enum':
            if arg.enum_items:
                arg_type = "Literal[" + ", ".join("'" + i[0] + "'" for i in arg.enum_items) + "]"
            else:
                arg_type = 'str'
            if arg.is_enum_flag:
                # Confusingly named flag, but indicates that the property is a set of enum values
                arg_type = "set[" + arg_type + "]"
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

        arg_type = type_name(arg_type, arg.fixed_type is not None)

        if is_return:
            return ReturnsLine(description=description, type=arg_type)
        else:
            return ArgumentLine(name=arg.identifier, description=description, type=arg_type)

    def get_return(returns: tuple[rna_info.InfoPropertyRNA, ...]) -> ReturnsLine:
        '''Helper function, that creates the return definition subdictionary ("@returns")
           Arguments:
           @returns (list of rna_info.InfoPropertyRNA): descriptor of the return values

           Returns: an definistion subdictionary (keys: type", "description")
        '''
        if len(returns) == 1:
            return get_argitem(returns[0], is_return=True)
        else: #many different values:
            description = "\n("
            for ret in returns:
                item = get_argitem(ret, is_return=True)
                description = description + "\n{0}{1}({2}):{3}".format(_IDENT, ret.identifier, item.type, item.description)
            #give just the description, not the type!
            description = description + "\n)"
            return ReturnsLine(description=description)

    definition_def = DefLine(description="")
    definition = DefinitionParts(definition_def=definition_def)

    if isinstance(info, rna_info.InfoStructRNA):
        py_class = info.py_class
        # base classes of this struct
        bases = []
        module_overrides = []

        if (
                is_rna_metaclass(py_class)
                # Skip if the module isn't included, otherwise we can't reference it
                and is_included(py_class)
                # Skip if the struct class is not accessible from its module
                and py_class.__module__ in sys.modules
                and hasattr(sys.modules[py_class.__module__], py_class.__qualname__)
        ):
            if py_class.__module__ != "bpy.types":
                # Structs that have a type that uses the bpy_struct_meta_idprop metaclass are often combined with an
                # existing type which is the real py_class that gets used at runtime. If we simply use the real type
                # directly in our documentation of bpy.types.<identifier>, e.g.
                #   "{identifier} = {py_class.__module__}.{py_class.__qualname__}"
                # or
                #   "from {py_class.__module__} import {py_class.__qualname__} as {identifier}"
                # PyCharm would miss all the extra properties and functions from the StructRNA that are added to the real
                # type at runtime.
                # As a workaround, we make a new bpy.types version of the struct that is a subclass of its real type.

                # Add the real type as a base (it should go before the real bases to match the method resolution order)
                bases.append(py_class)

                # skip any base classes that are superclasses of the metaclass, by comparing against the method resolution
                # order, to avoid duplicates
                metaclass_mro = set(py_class.mro())
                for py_base in py_class.__bases__:
                    if py_base not in metaclass_mro:
                        bases.append(py_base)
                module_overrides = [None] * len(bases)
                definition_def.is_fake_class = True
            else:
                for py_base in py_class.__bases__:
                    bases.append(py_base)
                    # If the base is a class we've replaced with a fake class extending the real class, change the base
                    # to the fake class
                    if is_rna_metaclass(py_base) and py_base.__module__ != "bpy.types":
                        module_overrides.append("bpy.types")
                    else:
                        module_overrides.append(None)
                definition_def.bases_fake = True
        else:
            bases = py_class.__bases__
            module_overrides = [None] * len(bases)

        # Sanity check that the bl_rna reported base is included in bases, either directly, or as a superclass
        # (the base type in the InfoStructRNA comes from py_class.bl_rna.base)
        if info.base:
            rna_base_type = info.base.py_class
            if rna_base_type:
                found_base = False
                for base in bases:
                    found_base |= issubclass(base, rna_base_type)
                if not found_base:
                    _ERRORS.append(f"bl_rna reported base type {rna_base_type} is missing from {py_class} bases:"
                                   f" {py_class.__bases__}")

        bases_strings = ",".join(map(get_full_type_name, bases, module_overrides))
        prototype = "class {0}({1}):".format(info.identifier, bases_strings)
        if definition_def.prototype is None:
            definition_def.prototype = prototype
        definition_def.description = info.description
        if definition_def.hint is None:
            definition_def.hint = "class"

    elif isinstance(info, rna_info.InfoPropertyRNA):
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
            if info.is_enum_flag:
                # Confusingly named flag, but indicates that the property is a set of enum values
                prop_type = "set[" + prop_type + "]"
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

        property_type_str = type_name(prop_type, info.fixed_type is not None, array_dimensions, info.is_readonly,
                                      built_in_type, collection_element_type)
        if extra_property_types and property_type_str not in extra_property_types:
            property_type_str = "Union[" + ", ".join(chain([property_type_str], extra_property_types)) + "]"
        prototype = "{0}: {1} = ...".format(
            info.identifier,
            property_type_str)
        if info.is_readonly:
            prototype = prototype + "  # (read only)"

        if definition_def.prototype is None:
            definition_def.prototype = prototype
        if definition_def.hint is None:
            definition_def.hint = "property"

        if info.description:
            definition_def.description = info.description

        if definition.returns_def is None:
            definition.returns_def = ReturnsLine(name="returns", description=info.get_type_description(as_ret=True))

    elif isinstance(info, rna_info.InfoFunctionRNA):
        # Add the classmethod decorator and append 'cls' to the argument for classmethod and append 'self' to the
        # arguments of functions that use it
        if info.is_classmethod:
            if definition_def.decorator is None:
                definition_def.decorator = "@classmethod\n"
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
        for i, arg in enumerate(info.args):
            if first_not_required is None:
                if not arg.is_required:
                    first_not_required = i
            elif first_required_after_not_required is None:
                if arg.is_required:
                    first_required_after_not_required = i
                    # The next argument to append doesn't have a default value, but it comes after arguments that do so
                    # '*' must be appended beforehand to signify that all arguments without default values after this
                    # point are required to be specified by name
                    arg_strings.append('*')
            # Append the next argument with its default value if it exists
            arg_line = get_argitem(arg)
            arg_string = f"{arg_line.name}: {arg_line.type}"
            # Add default argument if the argument is not required
            prop_default = arg.default_str
            if not arg.is_required and prop_default:
                arg_string += f" = {prop_default}"

            # Append argument to prototype arg strings
            arg_strings.append(arg_string)
            # Append argument to argument defs
            definition.argument_defs.setdefault(arg.identifier, arg_line)
        args_str = ", ".join(arg_strings)
        prototype = "{0}({1})".format(info.identifier, args_str)
        if definition_def.prototype is None:
            definition_def.prototype = prototype
        if definition_def.hint is None:
            definition_def.hint = "function"
        definition_def.description = info.description
        #append returns (operators have none):
        if info.return_values:
            definition.returns_def = get_return(info.return_values)

    elif isinstance(info, rna_info.InfoOperatorRNA):
        args_str = ", ".join(prop.get_arg_default(force=False) for prop in info.args)
        prototype = "{0}({1})".format(info.func_name, args_str)
        if definition_def.prototype is None:
            definition_def.prototype = prototype
        if definition_def.hint is None:
            definition_def.hint = "operator"
        if info.description and info.description != "(undocumented operator)":
            definition_def.description = info.description
        else: #just empty line
            definition_def.description = "undocumented"
        #append arguments:
        for arg in info.args:
            definition.argument_defs.setdefault(arg.identifier, get_argitem(arg))
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


def doc2definition(doc: Union[DefinitionParts, str, None], docstring_ident=_IDENT, module_name=None,
                   extra_docstring_lines=()) -> Definition:
    '''Method converts given doctext into declaration and docstring comment
    Details:
    @doc (DefinitionParts or string) - the documentation text of the member (preferably in sphinx RST syntax)
                            or ready dictionary of dictionaries, like the result of rst2list() (see above)
    @docstring_ident (string) - the amount of spaces before docstring markings
    Returns: Definition
    '''
    def format_arg(data: ArgumentLine):
        '''Returns line of text, describing an argument or return statement
            Arguments:
            data (dictionary): a "subdictionary" of <definition>, describing single item:
                                ("name", ["description"],["type"])
        '''
        return_type = data.type
        description = data.description
        if return_type:
            if description:
                return f"@{data.name} ({return_type}): {description}"
            else:
                return f"@{data.name} ({return_type}): <not documented>"
        else:
            if description:
                return f"@{data.name}: {description}"
            else:
                return f"@{data.name}: <not documented>"

    def guess_prefixed_type(type_name):
        if module_name and type_name.isalnum() and not hasattr(builtins, type_name):
            return f"{module_name}.{type_name}"
        else:
            # Most likely already contains the correct module
            return type_name

    if isinstance(doc, DefinitionParts):
        definition = doc
    elif isinstance(doc, str):
        definition = rst2list(doc)
    else:
        return Definition(docstring=docstring_ident + "\n")

    _returns = definition.returns_def
    rtype = _returns.type if _returns else None

    definition.returns_def = None

    _note = definition.note_def
    definition.note_def = None

    _seealso = definition.see_also_def
    definition.see_also_def = None

    _def = definition.definition_def
    definition.definition_def = None

    if _def:
        declaration = _def.prototype
        # function, method, classmethod or staticmethod
        function_type = _def.function_type
        decorator = _def.decorator
        hint = _def.hint
        if not rtype:
            rtype = _def.return_type
    else:
        declaration = None
        function_type = None
        decorator = None
        hint = None

    rtype_comment = None
    if rtype:
        # Some type aberrations have comments, which will need to go after the ":" at the end
        comment_start = rtype.find("#")
        if comment_start != -1:
            rtype_stripped = rtype[:comment_start].strip()
            rtype_stripped = guess_prefixed_type(rtype_stripped)
            rtype_comment = rtype[comment_start:]
            rtype = rtype_stripped
        else:
            rtype = guess_prefixed_type(rtype.strip())

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
                declaration = decorator + "def " + declaration + " -> " + rtype + ":"
            else:
                declaration = decorator + "def " + declaration + ":"
    elif rtype:
        rtype = guess_prefixed_type(rtype)

    ident = docstring_ident + _IDENT #all next row will have additional ident, to match the first line
    lines = [] #lines of the docstring text

    al = lines.append #trick, to re-use the write_indented_lines to add the line

    description = _def.description
    if description:
        write_indented_lines(ident, al, description, False)  # fill the <lines> list
        if lines:
            lines[0] = lines[0][len(ident):] #skip the ident in the first and the last line:
            #                                 (the docstring's prefix "   '''" will be placed there)

    if definition.argument_defs: #Are named arguments there?
        # Disabled, it seems to break docstings from showing in PyCharm's Quick Documentation
        #write_indented_lines(ident,al,"Arguments:",False)

        for dict_descr in definition.argument_defs.values(): #sort the lines in the original sequence
            #first item of the <tuple> is the key, second - the value (dictionary describing a single element)
            write_indented_lines(ident,al,format_arg(dict_descr),False)
        #end for
        al("\n")

    if _returns:
        write_indented_lines(ident, al, format_arg(_returns),False)

    if _note:
        note_description = _note.description
        if note_description:
            write_indented_lines(ident, al, "Note: " + note_description,False)

    if _seealso:
        seealso_description = _seealso.description
        if seealso_description:
            write_indented_lines(ident, al, "(seealso " + seealso_description+")\n", False)

    if not lines:
        lines.append("<not documented>\n")

    lines += extra_docstring_lines

    if _def.is_fake_class:
        write_indented_lines(ident, al, "This is a fake class added by pypredef_gen", False)

    if _def.bases_fake:
        write_indented_lines(ident, al, "This class has a fake class added by pypredef_gen in its bases", False)

    docstring = docstring_ident + "\"\"\"" + "".join(lines) + docstring_ident + "\"\"\"\n"
    result = Definition(docstring=docstring, declaration=declaration, returns=rtype, returns_comment=rtype_comment,
                        function_type=function_type)

    return result

def pyfunc2predef(ident, fw, identifier, py_func, attribute_defined_class=None) -> bool:
    ''' Creates declaration of a function or class method
        Details:
        @ident (string): the required prefix (spaces)
        @fw (function): the unified shortcut to print() or file.write() function
        @identifier (string): the name of the member
        @py_func (<py function>): the method, that is being described here
        @is_class (boolean): True, when it is a class member
    '''
    is_class = attribute_defined_class is not None
    if is_class:
        if identifier not in attribute_defined_class.__dict__:
            # Skip functions that are not defined on this class, as this indicates that they are defined in one of the
            # class' bases and not overridden
            return False

    # If the function is defined in a different module which is also included, reference the function
    func_module = getattr(py_func, '__module__', None)
    func_name = getattr(py_func, '__name__', None)
    attribute_defined_class_module = getattr(attribute_defined_class, '__module__', None)
    if (
            is_class and func_module and func_name
            and attribute_defined_class_module != func_module
            and is_included(func_module)
    ):
        func_self = getattr(py_func, '__self__', None)
        if func_self:
            func_self_module = func_self.__module__
            fw(ident + f"{identifier} = {func_self_module}.{func_self.__qualname__}.{py_func.__name__}\n\n")
        else:
            fw(ident + f"{identifier} = {func_module}.{py_func.__name__}\n\n")
        return True
    function_defined_class = getattr(py_func, '__self__', attribute_defined_class)
    try:
        signature = inspect.signature(py_func)
        if is_class:
            if len(signature.parameters) == 0:
                fw(ident + "@staticmethod\n")
            else:
                static_attr = inspect.getattr_static(function_defined_class, identifier)
                if isinstance(static_attr, staticmethod):
                    fw(ident + "@staticmethod\n")
                elif isinstance(static_attr, classmethod):
                    fw(ident + "@classmethod\n")

        definition = doc2definition(py_func.__doc__) #parse the eventual RST sphinx markup

        declaration = definition.declaration
        returns = definition.returns
        docstring = definition.docstring

        if declaration:
            write_indented_lines(ident, fw, declaration, False)
        else:
            arg_str = str(signature)
            # Set or List default arguments can end up like:
            #   types={<class 'Stroke'>, <class 'StrokeVertexIterator'>}
            arg_str = arg_str.replace("<class '", "").replace("'>", "")
            fmt = ident + "def %s%s"
            if returns:
                returns_comment = definition.returns_comment
                if returns_comment:
                    fmt += " -> " + returns + ":  " + returns_comment + "\n"
                else:
                    fmt += " -> " + returns + ":\n"
            else:
                fmt += ":\n"
            fw(fmt % (identifier, arg_str))

        if docstring:
            write_indented_lines(ident, fw, docstring, False)

        if returns:
            # Don't include "return <type>", simply use "..." which is a common convention for method stubs, the actual
            # return type is hinted in the definitions
            write_indented_lines(ident+_IDENT, fw, "...", False)
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

    if isinstance(descr, (types.GetSetDescriptorType, types.MemberDescriptorType)): #an attribute of the module or class
        definition = doc2definition(descr.__doc__,"", module_name=module_name) #parse the eventual RST sphinx markup
        returns = definition.returns
        returns_comment = definition.returns_comment
        if not returns:
            returns = "None"  # we have to assign just something, to be properly parsed!
        line = ident + identifier + ": " + returns + " = ..."
        if returns_comment:
            line += "  " + returns_comment
        fw(line + "\n")

        docstring = definition.docstring
        if docstring:
            write_indented_lines(ident, fw, docstring, False)

    elif isinstance(descr, types.MethodDescriptorType):
        py_c_func2predef(ident, fw, module_name, type_name, identifier, descr, True, func_type=FuncType.from_descr(descr))
    elif isinstance(descr, types.ClassMethodDescriptorType):
        py_c_func2predef(ident, fw, module_name, type_name, identifier, descr, True, func_type=FuncType.from_descr(descr))
    else:
        raise TypeError("type was not MemberDescriptiorType, GetSetDescriptorType, MethodDescriptorType or ClassMethodDescriptorType")
    fw("\n")


def py_c_func2predef(ident, fw, module_name, type_name, identifier, py_func, is_class=True, func_type=FuncType(FuncTypeEnum.FUNCTION)):
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
    doc_function_type = definition.function_type

    if func_type.is_class_method():
        # Add the @classmethod decorator
        fw(ident+"@classmethod\n")
        if doc_function_type == 'staticmethod':
            _WARNINGS.append(f"Documentation error: The __doc__ for {module_name}.{type_name}.{identifier} claims it is"
                             f" a {doc_function_type}, but by inspection, it is a classmethod")
    elif func_type.is_static_method():
        # Add the @staticmethod decorator
        fw(ident + "@staticmethod\n")
        if doc_function_type == 'classmethod':
            _WARNINGS.append(f"Documentation error: The __doc__ for {module_name}.{type_name}.{identifier} claims it is"
                             f" a {doc_function_type}, but by inspection, it is a staticmethod")
    elif func_type.is_instance_method():
        # We won't be picky if the documentation says it's a function instead, since it can be implied from whether it
        # belongs to a class
        if doc_function_type == "classmethod" or doc_function_type == "staticmethod":
            _WARNINGS.append(f"Documentation error: The __doc__ for {module_name}.{type_name}.{identifier} claims it is"
                             f" a {doc_function_type}, but by inspection, it is a method")
    elif func_type.is_function():
        # We won't be picky if the documentation says it's a method instead, since it can be implied from whether it
        # belongs to a class
        if doc_function_type == "classmethod" or doc_function_type == "staticmethod":
            _WARNINGS.append(f"Documentation error: The __doc__ for {module_name}.{type_name}.{identifier} claims it is"
                             f" a {doc_function_type}, but by inspection, it is a function")

    declaration = definition.declaration
    returns = definition.returns
    if declaration:
        if is_class:
            # Insert cls or self parameter
            if returns:
                no_args = "()->" in declaration.replace(" ", "")
            else:
                no_args = "():" in declaration.replace(" ", "")
            if func_type.is_class_method():
                if no_args:
                    declaration = declaration.replace("(", "(cls", 1)
                else:
                    declaration = declaration.replace("(", "(cls, ", 1)
            elif not func_type.is_static_method():
                if no_args:
                    declaration = declaration.replace("(", "(self", 1)
                else:
                    declaration = declaration.replace("(", "(self, ", 1)

        elif module_name == 'bpy.props' and hasattr(py_func, '__name__'):
            # This is a hack to add in type hints to bpy.props, it relies on extra typing imports added into the module
            if py_func.__name__ in _BPY_PROPS_RETURN_HINTS:
                if py_func.__name__ == 'PointerProperty':
                    declaration = declaration.replace("type=None", "type: _T = None")
                elif py_func.__name__ == 'CollectionProperty':
                    declaration = declaration.replace("type=None", "type: type[_T] = None")
                declaration = declaration.replace("):", ") -> " + _BPY_PROPS_RETURN_HINTS[py_func.__name__] + ":")
            elif py_func.__name__ == 'FloatVectorProperty':
                # FloatVectorProperty changes type based on the subtype and size arguments. There are too many different
                # combinations to
                # Need to get an ID type instance to be able to check the float vector property types, the scene is a
                # good candidate
                scene = bpy.context.scene
                extra_declarations = []

                all_type_str = set()

                # Get the default subtype string. Some redundancy is included in-case the documentation changes slightly
                # in the future
                subtype_re = re.compile(r"subtype\s*=\s*[\"']([^,]+)[\"']\s*,")
                default_subtype = subtype_re.search(declaration).group(1)
                # Get the default size value. Some redundancy is included in-case the documentation changes slightly in
                # the future
                size_re = re.compile(r"size\s*=\s*([^,]+)\s*,")
                default_size = int(size_re.search(declaration).group(1))

                for return_type, combinations in _find_float_vector_prop_types(scene).items():
                    if return_type == bpy.types.bpy_prop_array:
                        return_type_str = f"type[bpy.types._generic_prop_array[float]]"
                    else:
                        return_type_str = f"type[{get_full_type_name(return_type)}]"
                    all_type_str.add(return_type_str)
                    extra_declaration_common = declaration.replace("):", f") -> {return_type_str}:")
                    for subtypes_list, range_list in combinations.items():
                        subtype_arg_literal = "Literal[" + ", ".join(map(repr, subtypes_list)) + "]"
                        size_arg_literal = "Literal[" + ", ".join(map(repr, chain.from_iterable(range_list))) + "]"

                        # Overloads should only take default arguments when the default is available for that overload
                        # e.g. "size: Literal[1]" cannot have the default argument of 3
                        include_default_subtype = default_subtype in subtypes_list
                        if include_default_subtype:
                            subtype_overload_str = f"subtype: {subtype_arg_literal} = '\\1',"
                        else:
                            subtype_overload_str = f"subtype: {subtype_arg_literal},"

                        include_default_size = any(default_size in r for r in range_list)
                        if include_default_size:
                            size_overload_str = f"size: {size_arg_literal} = \\1,"
                        else:
                            size_overload_str = f"size: {size_arg_literal},"

                        if not include_default_subtype or not include_default_size:
                            # If we've removed a default argument, we need to insert a '*' before that parameter
                            # otherwise we end up with a non-default parameter after parameters with defaults.
                            # Fortunately, bpy.props.FloatVectorProperty has every parameter with a default argument, so
                            # the '*' can be inserted in-front of all the parameters.
                            extra_declaration = extra_declaration_common.replace('(', '(*, ', 1)
                        else:
                            extra_declaration = str(extra_declaration_common)

                        # Add the subtype overloads
                        extra_declaration = re.sub(subtype_re, subtype_overload_str, extra_declaration, count=1)

                        # Add the size overloads
                        extra_declaration = re.sub(size_re, size_overload_str, extra_declaration, count=1)

                        # Prepend the @overload decorator
                        extra_declaration = "@overload\n" + extra_declaration + " ..."
                        extra_declarations.append(extra_declaration)

                implementation_return_str = "Union[" + ", ".join(sorted(all_type_str)) + "]"
                implementation_declaration = declaration.replace("):", ") -> " + implementation_return_str + ":")
                # Combine the overload declarations with the original implementation declaration
                declaration = "\n".join(extra_declarations) + "\n" + implementation_declaration

        write_indented_lines(ident, fw, declaration, False)
    else:
        # *argv, when we do not know about its arguments
        arg_str = "(*argv)"
        try:
            # inspect.signature can parse .__text_signature__ if present
            arg_str = str(inspect.signature(py_func))
        except ValueError:
            pass
        if returns:
            fw(f"{ident}def {identifier}{arg_str} -> {returns}:\n")
        else:
            fw(f"{ident}def {identifier}{arg_str}:\n")

    docstring = definition.docstring
    if docstring:
        write_indented_lines(ident, fw, docstring, False)

    if returns:
        # Don't include "return <type>", simply use "..." which is a common convention for method stubs, the actual
        # return type is hinted in the definitions
        write_indented_lines(ident+_IDENT, fw, "...", False)
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
    returns = definition.returns
    if returns:
        declaration = identifier + " = " + returns
        returns_comment = definition.returns_comment
        if returns_comment:
            declaration += "  " + returns_comment
    else:
        declaration = identifier + " = None"    #we have to assign just something, to be properly parsed!

    # readonly properties use "data" directive, variables use "attribute" directive
    if py_prop.fset is None: declaration = declaration + "  # (readonly)"

    fw(ident + declaration + "\n")

    docstring = definition.docstring
    if docstring:
        write_indented_lines(ident, fw, docstring, False)

    fw(ident + "\n")


def guess_prefixed_class_name(module_name, clazz):
    module = clazz.__module__
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
    if module_name in sys.modules and getattr(sys.modules[module_name], '__file__', None):
        try:
            # Normally this is only used by bl_ui.UI_UL_list since most classes with available source will have their
            # entire module available, in which case the entire module is copied and printing individual attributes like
            # classes is skipped. In a few cases, mainly for top-level modules such as bl_ui, we have to add extra
            # imports in the code where the imports would normally be added programmatically, otherwise PyCharm won't be
            # able to pick up on them
            src_str = inspect.getsource(value)
            fw(src_str)
            return
        except TypeError:
            pass

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
    docstring = definition.docstring
    if docstring:
        write_indented_lines("", fw, docstring, False)

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
        if isinstance(descr, (staticmethod, classmethod)):
            func_type = FuncType.from_descr(descr)
            # getattr(value, key) would return the function too, but it will be bound in the case of a classmethod
            # Usually the function will be a py_function, but sometimes it can be a built-in function/method too
            descr = descr.__func__

        else:
            func_type = None

        element_pair = (key, descr)

        if isinstance(descr, types.ClassMethodDescriptorType):
            py_class_method_descriptors.append(element_pair)
        elif isinstance(descr, types.MethodDescriptorType):
            py_method_descriptors.append(element_pair)
        elif isinstance(descr, (types.FunctionType, types.MethodType)):
            py_functions.append(element_pair)
        elif isinstance(descr, types.GetSetDescriptorType):
            py_getset_descriptors.append(element_pair)
        elif isinstance(descr, property):
            py_properties.append(element_pair)
        elif isinstance(descr, types.MemberDescriptorType):
            py_member_descriptors.append(element_pair)
        elif isinstance(descr, (types.BuiltinFunctionType, types.BuiltinMethodType)):
            py_c_functions.append((key, descr, func_type))
        elif isinstance(descr, types.WrapperDescriptorType):
            py_wrapper_descriptors.append(element_pair)
        elif isinstance(descr, (bool, int, float, str, bytes, NoneType)):
            class_attributes.append(element_pair)
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

    for key, descr, func_type in py_c_functions:
        py_c_func2predef(_IDENT, fw, module_name, type_name, key, descr, is_class=True, func_type=func_type)

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
    # Similarly, bl_ui and bl_operators we need to replace to allow for setting up their submodules in a
    # non-programmatic manner so that PyCharm can find them
    if hasattr(module, '__file__') and true_module_name not in {'bpy', 'bl_ui', 'bl_operators'}:
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
    fw(definition.docstring)
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
    top_level_module = get_top_level_module(relative_name)
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
           "\nfrom typing import TypeVar, Annotated, overload  # added by pypredef_gen"
           "\n_T = TypeVar('_T')  # added by pypredef_gen\n\n")

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
            # Check if every individual element is printable
            if all(map(is_printable_tuple_element, value)):
                # Blender uses some c-defined tuples (PyStructSequence) that are similar to namedtuples
                if is_tuple_pystructsequence(value):
                    print_pystructsequence("", fw, attribute, value)
                elif type(value) == tuple:
                    fw(f"{attribute} = {printable_tuple_repr(value)} # instance value \n\n")
                else:
                    # Other tuple subclasses may not be printable as-is, so create a regular tuple from it and print
                    # that instead
                    fw(f"{attribute} = {printable_tuple_repr(tuple(value))} # instance value: {repr(value)} \n\n")
            else:
                # It's difficult to reliably print elements of other types
                elements_text = " (elements omitted)" if value else ""
                fw(f"{attribute} = {type(value).__qualname__}() # instance value{elements_text} \n\n")

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

def rna_property2predef(ident, fw, descr, alternative_types=(), extra_docstring=()):
    ''' Creates declaration of a property
        Details:
        @ident (string): the required prefix (spaces)
        @fw (function): the unified shortcut to print() or file.write() function
        @descr (rna_info.InfoPropertyRNA): descriptor of the property
    '''
    def_parts = rna2list(descr, extra_property_types=alternative_types)
    definition = doc2definition(def_parts, docstring_ident="", extra_docstring_lines=extra_docstring)
    write_indented_lines(ident, fw, definition.declaration, False)

    docstring = definition.docstring
    if docstring:
        write_indented_lines(ident, fw, docstring, False)

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
        return_description = ("\nRUNNING_MODAL Running Modal – Keep the operator running with blender.\n"
                             "CANCELLED Cancelled – The operator exited without doing anything, so no undo entry should be pushed.\n"
                             "FINISHED Finished – The operator exited after completing its action.\n"
                             "PASS_THROUGH Pass Through – Do nothing and pass the event on.\n"
                             "INTERFACE Interface – Handled but not executed (popup menus).")
        if definition.returns_def is None:
            definition.returns_def = ReturnsLine(description=return_description, type=bpy_op_return)

    definition = doc2definition(definition)

    declaration = definition.declaration
    if is_bpy_op:
        # bpy.op functions take 3 optional positional arguments, in the form of
        # override_context: dict, execution_context: str, undo: bool
        # but these cannot be specified by name, so we must present them as *args
        # Insert the *args parameter into the declaration
        no_existing_args = declaration.find('()') != -1
        declaration = declaration.replace('(', "(*args: Union[dict, str, bool]" if no_existing_args else "(*args: Union[dict, str, bool], ", 1)

    write_indented_lines(ident,fw,declaration,False) #may contain two lines: decorator and declaration

    docstring = definition.docstring
    if docstring:
        write_indented_lines(ident, fw, docstring, False)

    returns = definition.returns
    if returns:
        # Don't include "return <type>", simply use "..." which is a common convention for method stubs, the actual
        # return type is hinted in the definitions
        write_indented_lines(ident+_IDENT, fw, "...", False)
    else:
        write_indented_lines(ident+_IDENT, fw, "pass", False)

    fw("\n")


def is_rna_metaclass(py_class):
    """Helper function to check if an rna_struct's py_class is a metaclass using class or is otherwise combined with a
    Python base class"""
    # Generally the base metaclass using types are in bpy_types, but subclasses could be anywhere
    if isinstance(py_class, (bpy.types.bpy_struct_meta_idprop, bpy_types.RNAMeta)):
        return True
    # The class Context, Gizmo, GizmoGroup, Macro, MeshEdge, MeshLoopTriangle and MeshPolygon don't use a metaclass, yet
    # they still become the base class for an rna_struct.
    # Perhaps there is a better way to detect such cases. We can't check if the class is a python defined class (has the
    # __code__ attribute) because that would also pick up subclasses such as most UI Panels and Menus
    if py_class.__module__ == 'bpy_types':
        return True
    else:
        return False


def write_context_properties(ident: str, fw: Callable[[str], None], rna_properties: list[rna_info.InfoPropertyRNA]):
    # Context attributes copied from https://docs.blender.org/api/current/bpy.context.html as of Blender 3.3.1
    buttons_context_attributes = {
        "texture_slot": "TextureSlot",
        "scene": "Scene",
        "world": "World",
        "object": "Object",
        "mesh": "Mesh",
        "armature": "Armature",
        "lattice": "Lattice",
        "curve": "Curve",
        "meta_ball": "MetaBall",
        "light": "Light",
        "speaker": "Speaker",
        "lightprobe": "LightProbe",
        "camera": "Camera",
        "material": "Material",
        "material_slot": "MaterialSlot",
        "texture": "Texture",
        "texture_user": "ID",
        "texture_user_property": "Property",
        "bone": "Bone",
        "edit_bone": "EditBone",
        "pose_bone": "PoseBone",
        "particle_system": "ParticleSystem",
        "particle_system_editable": "ParticleSystem",
        "particle_settings": "ParticleSettings",
        "cloth": "ClothModifier",
        "soft_body": "SoftBodyModifier",
        "fluid": "FluidSimulationModifier",
        "collision": "CollisionModifier",
        "brush": "Brush",
        "dynamic_paint": "DynamicPaintModifier",
        "line_style": "FreestyleLineStyle",
        "collection": "LayerCollection",
        "gpencil": "GreasePencil",
        "curves": "Curves",
        "volume": "Volume",
    }
    clip_context_attributes = {
        "edit_movieclip": "MovieClip",
        "edit_mask": "Mask",
    }
    file_context_attributes = {
        "active_file": "FileSelectEntry",
        "selected_files": ["FileSelectEntry"],
        "asset_library_ref": "AssetLibraryReference",
        "selected_asset_files": "FileSelectEntry",
        "id": "ID",
    }
    image_context_attributes = {
        "edit_image": "Image",
        "edit_mask": "Mask",
    }
    node_context_attributes = {
        "selected_nodes": ["Node"],
        "active_node": "Node",
        "light": "Light",
        "material": "Material",
        "world": "World",
    }
    screen_context_attributes = {
        "scene": "Scene",  # scene is already available in all contexts though...
        "view_layer": "ViewLayer",  # view_layer is already available in all contexts though...
        "visible_objects": ["Object"],
        "selectable_objects": ["Object"],
        "selected_objects": ["Object"],
        "editable_objects": ["Object"],
        "selected_editable_objects": ["Object"],
        "objects_in_mode": ["Object"],
        "objects_in_mode_unique_data": ["Object"],
        "visible_bones": ["EditBone"],
        "editable_bones": ["EditBone"],
        "selected_bones": ["EditBone"],
        "selected_editable_bones": ["EditBone"],
        "visible_pose_bones": ["PoseBone"],
        "selected_pose_bones": ["PoseBone"],
        "selected_pose_bones_from_active_object": ["PoseBone"],
        "active_bone": "EditBone",
        "active_pose_bone": "PoseBone",
        "active_object": "Object",
        "object": "Object",
        "edit_object": "Object",
        "sculpt_object": "Object",
        "vertex_paint_object": "Object",
        "weight_paint_object": "Object",
        "image_paint_object": "Object",
        "particle_edit_object": "Object",
        "pose_object": "Object",
        "active_sequence_strip": "Sequence",
        "sequences": ["Sequence"],
        "selected_sequences": ["Sequence"],
        "selected_editable_sequences": ["Sequence"],
        "active_nla_track": "NlaTrack",
        "active_nla_strip": "NlaStrip",
        "selected_nla_strips": ["NlaStrip"],
        "selected_movieclip_tracks": ["MovieTrackingTrack"],
        "gpencil_data": "GreasePencil",
        "gpencil_data_owner": "ID",
        "annotation_data": "GreasePencil",
        "annotation_data_owner": "ID",
        "visible_gpencil_layers": ["GPencilLayer"],
        "editable_gpencil_layers": ["GPencilLayer"],
        "editable_gpencil_strokes": ["GPencilStroke"],
        "active_gpencil_layer": ["GPencilLayer"],  # Layer singular, but is a sequence?
        "active_gpencil_frame": ["GPencilLayer"],  # Layer singular, but is a sequence? # Documented as GreasePencilLayer
        "active_annotation_layer": "GPencilLayer",
        "active_operator": "Operator",
        "active_action": "Action",
        "selected_visible_actions": ["Action"],
        "selected_editable_actions": ["Action"],
        "visible_fcurves": ["FCurve"],
        "editable_fcurves": ["FCurve"],
        "selected_visible_fcurves": ["FCurve"],
        "selected_editable_fcurves": ["FCurve"],
        "active_editable_fcurves": "FCurve",
        "selected_editable_keyframes": ["Keyframe"],
        "ui_list": "UIList",
        "asset_library_ref": "AssetLibraryReference",
    }
    sequencer_context_attributes = {
        "edit_mask": "Mask",
    }
    text_context_attributes = {
        "edit_text": "Text",
    }
    view3d_context_attributes = {
        "active_object": "Object",
        "selected_ids": ["ID"],
    }
    context_attributes = [
        (buttons_context_attributes, "Buttons"),
        (clip_context_attributes, "Clip"),
        (file_context_attributes, "File"),
        (image_context_attributes, "Image"),
        (node_context_attributes, "Node"),
        (screen_context_attributes, "Screen"),
        (sequencer_context_attributes, "Sequencer"),
        (text_context_attributes, "Text"),
        (view3d_context_attributes, "View3D"),
    ]
    always_available_attributes = {prop.identifier: prop for prop in rna_properties}
    all_context_attributes: dict[str, tuple[set[str], set[str]]] = {}
    conflict_attributes: dict[str, list[tuple[str, str]]] = defaultdict(list)
    for attributes_dict, context_type in context_attributes:
        for attribute_name, attribute_type_str in attributes_dict.items():
            is_sequence = False
            if isinstance(attribute_type_str, list):
                is_sequence = True
                attribute_type_str = attribute_type_str[0]

            if not hasattr(bpy.types, attribute_type_str):
                warning_part = "in a sequence as" if is_sequence else "as"
                _WARNINGS.append(f"Could not find bpy.types.{attribute_type_str} available {warning_part}"
                                 f" bpy.types.Context.{attribute_name} for '{context_type}' contexts (it may be"
                                 f" from a newer Blender version)")
                continue

            processed_attributes_dict = all_context_attributes
            conflict = False
            # Check if the attribute is always available due to being an rna property
            if attribute_name in always_available_attributes:
                existing_attribute = always_available_attributes[attribute_name]
                # If the type matches, we'll just skip it
                if existing_attribute.fixed_type and existing_attribute.fixed_type.identifier == attribute_type_str:
                    print(f"Skipping bpy.types.Context attribute {attribute_name} from {context_type} contexts as it is"
                          f" available in all contexts with the same type")
                    continue
                # If the type doesn't match, add it to the conflict dict instead. Conflicts information will be added to
                # the standard rna property
                else:
                    conflict = True

            attribute_type_str = "bpy.types." + attribute_type_str
            if is_sequence:
                attribute_type_str = f"Sequence[{attribute_type_str}]"

            if conflict:
                conflict_attributes[attribute_name].append((attribute_type_str, context_type))
            else:
                types_set, context_type_set = all_context_attributes.setdefault(attribute_name, (set(), set()))
                types_set.add(attribute_type_str)
                context_type_set.add(context_type)

    for prop in rna_properties:
        if prop.identifier in conflict_attributes:
            type_context_type_pairs = conflict_attributes[prop.identifier]
            extra_docstring_lines = []
            for type_str, context_type in type_context_type_pairs:
                extra_docstring_lines.append(f"When the context is a {context_type} context, the type is {type_str}\n")
            rna_property2predef(ident, fw, prop, alternative_types=[p[0] for p in type_context_type_pairs],
                                extra_docstring=extra_docstring_lines)
        else:
            rna_property2predef(ident, fw, prop)

    for attribute_name, (attribute_types, attribute_contexts) in sorted(all_context_attributes.items(), key=lambda t: t[0]):
        if attribute_name in conflict_attributes:
            # Already covered by the main attribute documentation
            continue

        if len(attribute_types) > 1:
            # There currently aren't any attributes in multiple context types that can have a different type in
            # different contexts, there probably won't be in the future either, since it's confusing
            type_str = f"Union[{', '.join(sorted(attribute_types))}]"
        else:
            type_str = next(iter(attribute_types))
        if len(attribute_contexts) > 1:
            sorted_contexts = sorted(attribute_contexts)
            contexts_str_part = ', '.join(sorted_contexts[:-1]) + " and " + sorted_contexts[-1]
        else:
            contexts_str_part = next(iter(attribute_contexts))
        contexts_str = contexts_str_part + " contexts only"
        lines = (
            # PyCharm won't show the docstring in Quick Documentation unless there's an assigned value, so we assign
            # to ...
            f"{attribute_name}: {type_str} = ...  # (read-only)\n"
            f'"""{contexts_str}"""\n'
        )
        write_indented_lines(ident, fw, lines)


def rna_struct2predef(ident, fw, descr: rna_info.InfoStructRNA, is_fake_module=False, module_name=None):
    ''' Creates declaration of a bpy structure
        Details:
        @ident (string): the required prefix (spaces)
        @fw (function): the unified shortcut to print() or file.write() function
        @descr (rna_info.InfoStructRNA): the descriptor of a Blender Python class
        @fake_module (bool): if true, print only the properties and functions without indentation and return attribute names
    '''

    is_external_py_metaclass = False
    if not is_fake_module:
        py_class = descr.py_class
        class_module = py_class.__module__
        if module_name and module_name != class_module:
            # Make sure it isn't a custom metaclass type that usually combines the struct with a class in bpy_types,
            # otherwise we lose all the properties/etc. defined in the struct
            class_module_prefix = get_top_level_module(class_module)
            # Can the class actually be accessed from the module it claims it belongs to?
            accessible = class_module in sys.modules and hasattr(sys.modules[class_module], py_class.__qualname__)
            if not is_rna_metaclass(py_class) and accessible:
                if class_module_prefix in INCLUDE_MODULES:
                    if descr.identifier == py_class.__name__:
                        fw(ident + f"from {class_module} import {py_class.__name__}\n")
                    else:
                        fw(ident + f"from {class_module} import {py_class.__name__} as {descr.identifier}\n")
                    return
                else:
                    if descr.identifier == py_class.__name__:
                        fw(f"\n# {descr.identifier} is accessible in {class_module}, but {class_module} is not an"
                           f" included module."
                           f"\n# {descr.identifier} has been reproduced here as a stub.\n")
                    else:
                        fw(f"\n# {descr.identifier} is accessible in {class_module} as {py_class.__name__}, but"
                           f" {class_module} is not an included module."
                           f"\n# {descr.identifier} has been reproduced here as a stub.\n")
            else:
                if not accessible:
                    fw(f"\n# {descr.identifier} claims to be accessible as {class_module}.{py_class.__qualname__}, but"
                       f" it could not be found\n")
                else:
                    fw("\n")
                if class_module in sys.modules and getattr(sys.modules[class_module], '__file__', None):
                    is_external_py_metaclass = True
        else:
            fw("\n")

        print("class %s:" % descr.identifier)
        definition = doc2definition(rna2list(descr), module_name=module_name)
        write_indented_lines(ident, fw, definition.declaration, False)

        docstring = definition.docstring
        if docstring:
            write_indented_lines(ident, fw, docstring, False)

        #native properties
        ident = ident + _IDENT

    rna_properties = descr.properties
    rna_properties.sort(key= lambda prop: prop.identifier)
    # Extra properties for bpy.types.Context
    if descr.identifier == 'Context' and descr.py_class == bpy.types.Context:
        write_context_properties(ident, fw, rna_properties)
    else:
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
    # If it's a fake module, there's no inheritance due to it not being a class, so we can't check against the class to
    # see if we need to include the function or not
    owning_class = None if is_fake_module else descr.py_class
    for identifier, function in py_functions:
        # If the class is a metaclass, exists in a python file and the function does too, skip it as the class in
        # bpy.types will be faked and inherit from the metaclass
        if is_external_py_metaclass and hasattr(function, '__code__'):
            continue
        pyfunc2predef(ident, fw, identifier, function, attribute_defined_class=owning_class)

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
        # Fake _bpy module so that copied bpy_types mostly works
        _bpy_path = os.path.join(BASEPATH, "_bpy.py")
        file = open(_bpy_path, "w")
        file.write(
            '"""Fake _bpy module so that bpy_types works"""\n'
            "import bpy.types as types\n")
        file.close()

        INCLUDE_MODULES.append('bpy_types')

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
    def __getitem__(self, key: _prop_collection_key) -> _T: ...
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
    @overload
    def __getitem__(self, key) -> _T: ...
    def __iter__(self) -> Iterator[_T]: ...

class _generic_prop_collection_idprop(_generic_prop_collection[_T], {bpy_prop_collection_idprop_name}):
    \"\"\"Fake generic version of bpy_prop_collection_idprop (not available from within Blender) added by pypredef_gen\"\"\"
    def add(self) -> _T: ...
    def remove(self, key: int, /): ...
    def move(self, key: int, pos: int, /): ...
    def clear(self): ...

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
            # Only include classes accessible from bpy.types and from included modules
            if hasattr(bpy.types, struct.identifier) and is_included(struct.py_class):
                # Separate structs by their top-level base as a means to separate them into multiple smaller files
                structs_by_base[get_top_level_base(struct)].append(struct)

        for base_name, structs_list in structs_by_base.items():
            base_module_name = "_types_" + base_name.lower()
            fw(f"from .{base_module_name} import {', '.join(s.identifier for s in structs_list)}\n")

            base_module_filepath = os.path.join(types_dirpath, f"{base_module_name}.py")
            base_module_file = open(base_module_filepath, "w")
            bm_fw = base_module_file.write
            bm_fw(f'"""fake bpy.types submodule for {base_name} types"""\n\n')

            bm_fw("from ... import bpy\n")
            if "mathutils" in INCLUDE_MODULES:
                bm_fw("from ... import mathutils\n")
            if "bl_ui" in INCLUDE_MODULES:
                bm_fw("from ... import bl_ui\n")
            if "nodeitems_utils" in INCLUDE_MODULES:
                bm_fw("import nodeitems_utils\n")
            if "bl_operators" in INCLUDE_MODULES:
                bm_fw("from ... import bl_operators\n")
            if "bpy_types" in INCLUDE_MODULES:
                bm_fw("import bpy_types\n")

            if _ADDON_MODULES_INCLUDED:
                bm_fw(f"import {', '.join(ADDON_MODULES)}\n")
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

    if 'cycles' in INCLUDE_MODULES and '_cycles' not in EXCLUDE_MODULES:
        # Include built-in _cycles module when cycles module is included
        INCLUDE_MODULES.append("_cycles")

    for module_name in INCLUDE_MODULES:
        if module_name not in EXCLUDE_MODULES:
            try:
                module = importlib.import_module(module_name)
            except ModuleNotFoundError as mnfe:
                _ERRORS.append(str(mnfe))
            else:
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

        # TODO: Option to include addon modules, default to False because they're not really referenced outside of Panel
        #  functions
        if _ADDON_MODULES_INCLUDED:
            INCLUDE_MODULES.extend(ADDON_MODULES)

        args = parser.parse_args(argv)

        if args.external_rst_dir:
            load_external_rst(args.external_rst_dir)

        _ARG_SKIP_FILES = args.skip_files

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

        # Print warnings and errors
        if bpy.app.background:
            print("\nSome graphics dependent types and modules (mainly the bgl module) may not be loaded fully when"
                  " running with the -b argument. If you need full predefinition files for graphics related types and"
                  " modules, run the script without the -b argument")
        if _WARNINGS:
            print("Warnings:\n\t", end="")
            print(*_WARNINGS, sep="\n\t", end="\n\n")
        if _ERRORS:
            print("Errors:\n\t", end="")
            print(*_ERRORS, sep="\n\t", end="\n\n")

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


main() #just run it! Unconditional call makes it easier to debug Blender script in Eclipse,
#       (using pydev_debug.py). It's doubtful, that it will be imported as additional module.


if __name__ == '__main__':
    sys.exit() #Close Blender, when you run it from the command line....
