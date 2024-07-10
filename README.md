# dll_invoke
# 1.安装依赖

```Python
pip install -r requirements.txt
```

# 2. 使用方法

## 2.1 查看函数列表

1. 打开cmd窗口

2. 输入命令：``curl -H "apiKey: a1234567890" ``http://127.0.0.1:5000/functions````

3. 输出：

   1. ```Python
      [
        {
          "func_id": 1,
          "func_name": "GD_HYDROSTATIC_PRESSURE"
        }
      ]
      ```

4. 展示：

![image](https://github.com/xxhanzo/dll_invoke/assets/97886040/abe87941-b7cb-4124-b404-074d5b8a7dfa)

## 2.2 查看函数参数

1. 输入命令：``curl -H "apiKey: a1234567890" ``http://127.0.0.1:5000/functions/1````

2. 输出：

   1. ```Python
      {
        "func_name": "GD_HYDROSTATIC_PRESSURE",
        "params": [
          [
            "double[]",
            "A"
          ],
          [
            "double[]",
            "B"
          ],
          [
            "double[]",
            "C"
          ],
          [
            "double[]",
            "D"
          ],
          [
            "double[]",
            "E"
          ],
          [
            "double[]",
            "F"
          ],
          [
            "ref double",
            "WL_Up"
          ],
          [
            "ref double",
            "WL_Down"
          ],
          [
            "ref double",
            "Density"
          ],
          [
            "ref double",
            "Acceleration"
          ],
          [
            "double[]",
            "A_F"
          ],
          [
            "double[]",
            "F_E"
          ],
          [
            "double[]",
            "C_D"
          ],
          [
            "double[]",
            "B_C"
          ],
          [
            "double[,]",
            "AREA_AF"
          ],
          [
            "double[,]",
            "AREA_FE"
          ],
          [
            "double[,]",
            "AREA_CD"
          ],
          [
            "double[,]",
            "AREA_BC"
          ]
        ]
      }
      ```

3. 展示：（过长只展示部分）

![image](https://github.com/xxhanzo/dll_invoke/assets/97886040/c72db4e0-1dc1-4be2-b0c3-34712c238afb)

## 2.3 调用函数

1. 输入对应的参数:

   1. ```Python
      curl -X POST http://127.0.0.1:5000/functions/1/call -H "Content-Type: application/json" -H "apiKey: a1234567890" -d "{\"A\": [0, 100], \"B\": [10, 100], \"C\": [10, 85], \"D\": [80, 0], \"E\": [-5, 0], \"F\": [0, 20], \"WL_Up\": 10, \"WL_Down\": 110, \"Density\": 1000, \"Acceleration\": 9.8}"
      ```

2. 输出：

```Python
{
  "AREA_AF": [
    [
      0.0,
      0.0,
      0.0,
      0.0,
      0.0
    ],
    [
      0.0,
      0.0,
      0.0,
      0.0,
      0.0
    ]
  ],
  "AREA_BC": [
    [
      10.0,
      10.0,
      14.9,
      11.96,
      10.0
    ],
    [
      100.0,
      85.0,
      85.0,
      100.0,
      100.0
    ]
  ],
  "AREA_CD": [
    [
      10.0,
      80.0,
      96.6428158799036,
      13.782458154523544,
      10.0
    ],
    [
      85.0,
      0.0,
      13.705848371685308,
      88.1149655390194,
      85.0
    ]
  ],
  "AREA_FE": [
    [
      -2.5,
      -5.0,
      -6.9014793002848505,
      -2.5,
      -2.5
    ],
    [
      10.0,
      0.0,
      0.4753698250712126,
      10.0,
      10.0
    ]
  ],
  "A_F": [
    0.0,
    0.0,
    0.0,
    0.0
  ],
  "B_C": [
    2572.499999999998,
    10.0,
    91.4285714285715,
    -180.0
  ],
  "C_D": [
    72840.1316668644,
    52.34567900572438,
    33.58024690809543,
    -140.52754015165618
  ],
  "F_E": [
    505.08043913817403,
    -4.166666665794764,
    3.333333333115661,
    -14.036243467926479
  ]
}
```

1. 展示（过长只展示对应结果部分）及和原图数据对比：

![image](https://github.com/xxhanzo/dll_invoke/assets/97886040/3c25f37f-95df-4cc8-9427-d7a9d10adc86)

![image](https://github.com/xxhanzo/dll_invoke/assets/97886040/044dbdf9-56c2-4331-927d-2440439a9918)


# 3. 使用流程图

![whiteboard_exported_image (1)](https://github.com/xxhanzo/dll_invoke/assets/97886040/207c6c88-7342-4530-9fac-1522a73fdea2)
