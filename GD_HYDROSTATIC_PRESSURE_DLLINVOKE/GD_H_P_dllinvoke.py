import os
import sys
import pefile
from flask import Flask, request, jsonify, abort
import ctypes
from functools import wraps

app = Flask(__name__)

API_KEY = "a1234567890"

# DLL 文件路径
dll_path = os.path.abspath(
    './data/GD_HYDROSTATIC_PRESSURE/GD_HYDROSTATIC_PRESSURE/x64/Release/GD_HYDROSTATIC_PRESSURE.dll')
dll_directory = os.path.dirname(dll_path)

# 将 DLL 所在目录添加到系统路径中
os.environ['PATH'] = dll_directory + os.pathsep + os.environ['PATH']


# 获取 DLL 文件中的函数列表
def list_functions(dll_path):
    if not os.path.isfile(dll_path):
        print("文件路径无效。")
        return []

    try:
        pe = pefile.PE(dll_path)
        functions = []
        for entry in pe.DIRECTORY_ENTRY_EXPORT.symbols:
            functions.append((entry.ordinal, entry.name.decode('utf-8')))
        return functions
    except Exception as e:
        print(f"无法加载文件: {e}")
        return []


functions = list_functions(dll_path)
func_id_map = {i + 1: name for i, (_, name) in enumerate(functions)}


def require_api_key(f):
    @wraps(f)
    def decorated_function(*args, **kwargs):
        api_key = request.headers.get('apiKey')
        if api_key != API_KEY:
            abort(401)
        return f(*args, **kwargs)

    return decorated_function


@app.route('/functions', methods=['GET'])
@require_api_key
def get_functions():
    response = [{"func_id": func_id, "func_name": name} for func_id, name in func_id_map.items()]
    return jsonify(response)


@app.route('/functions/<int:func_id>', methods=['GET'])
@require_api_key
def get_function_details(func_id):
    func_name = func_id_map.get(func_id)
    if not func_name:
        return jsonify({"error": "Function ID not found"}), 404

    # 假设我们已经解析了参数信息，返回一个示例
    params = [
        ("double[]", "A"),
        ("double[]", "B"),
        ("double[]", "C"),
        ("double[]", "D"),
        ("double[]", "E"),
        ("double[]", "F"),
        ("ref double", "WL_Up"),
        ("ref double", "WL_Down"),
        ("ref double", "Density"),
        ("ref double", "Acceleration"),
        ("double[]", "A_F"),
        ("double[]", "F_E"),
        ("double[]", "C_D"),
        ("double[]", "B_C"),
        ("double[,]", "AREA_AF"),
        ("double[,]", "AREA_FE"),
        ("double[,]", "AREA_CD"),
        ("double[,]", "AREA_BC")
    ]
    response = {"func_name": func_name, "params": params}
    return jsonify(response)


@app.route('/functions/<int:func_id>/call', methods=['POST'])
@require_api_key
def call_function(func_id):
    try:
        func_name = func_id_map.get(func_id)
        if not func_name:
            return jsonify({"error": "Function ID not found"}), 404

        data = request.json
        params = [
            ("double[]", "A"),
            ("double[]", "B"),
            ("double[]", "C"),
            ("double[]", "D"),
            ("double[]", "E"),
            ("double[]", "F"),
            ("ref double", "WL_Up"),
            ("ref double", "WL_Down"),
            ("ref double", "Density"),
            ("ref double", "Acceleration"),
            ("double[]", "A_F"),
            ("double[]", "F_E"),
            ("double[]", "C_D"),
            ("double[]", "B_C"),
            ("double[,]", "AREA_AF"),
            ("double[,]", "AREA_FE"),
            ("double[,]", "AREA_CD"),
            ("double[,]", "AREA_BC")
        ]

        # 转换输入参数
        def to_double_array(data):
            return (ctypes.c_double * len(data))(*data)

        def to_double_ref(value):
            return ctypes.c_double(value)

        def to_double_2d_array(data):
            flat_data = [item for sublist in data for item in sublist]
            return (ctypes.c_double * len(flat_data))(*flat_data)

        # 创建输出参数
        A_F = (ctypes.c_double * 4)()
        F_E = (ctypes.c_double * 4)()
        C_D = (ctypes.c_double * 4)()
        B_C = (ctypes.c_double * 4)()
        AREA_AF = (ctypes.c_double * 10)()
        AREA_FE = (ctypes.c_double * 10)()
        AREA_CD = (ctypes.c_double * 10)()
        AREA_BC = (ctypes.c_double * 10)()

        # 加载 DLL 并获取函数
        dll = ctypes.CDLL(dll_path, winmode=0)
        func = getattr(dll, func_name)

        # 准备函数参数
        func.argtypes = [
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double),
            ctypes.POINTER(ctypes.c_double)
        ]

        # 调用 DLL 函数
        func(
            to_double_array(data['A']),
            to_double_array(data['B']),
            to_double_array(data['C']),
            to_double_array(data['D']),
            to_double_array(data['E']),
            to_double_array(data['F']),
            ctypes.byref(to_double_ref(data['WL_Up'])),
            ctypes.byref(to_double_ref(data['WL_Down'])),
            ctypes.byref(to_double_ref(data['Density'])),
            ctypes.byref(to_double_ref(data['Acceleration'])),
            A_F,
            F_E,
            C_D,
            B_C,
            AREA_AF,
            AREA_FE,
            AREA_CD,
            AREA_BC
        )

        # 准备输出结果
        def from_double_array(c_array, length):
            return [c_array[i] for i in range(length)]

        def from_double_2d_array(c_array, rows, cols):
            return [[c_array[i * cols + j] for j in range(cols)] for i in range(rows)]

        response = {
            "A_F": from_double_array(A_F, 4),
            "F_E": from_double_array(F_E, 4),
            "C_D": from_double_array(C_D, 4),
            "B_C": from_double_array(B_C, 4),
            "AREA_AF": from_double_2d_array(AREA_AF, 2, 5),
            "AREA_FE": from_double_2d_array(AREA_FE, 2, 5),
            "AREA_CD": from_double_2d_array(AREA_CD, 2, 5),
            "AREA_BC": from_double_2d_array(AREA_BC, 2, 5)
        }

        return jsonify(response)

    except KeyError as e:
        return jsonify({"error": f"Missing parameter: {e}"}), 400
    except AttributeError as e:
        return jsonify({"error": f"Function call failed: {e}"}), 500
    except Exception as e:
        return jsonify({"error": f"Unexpected error: {e}"}), 500


if __name__ == '__main__':
    app.run(debug=True)
