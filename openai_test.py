from openai import OpenAI
import base64

client = OpenAI()

print('#### 查看可用模型')
print(client.models.list())

#### 是否使用流式输出
stream = False

print('#### 文本')
completion = client.chat.completions.create(
    model="gpt-4o-2024-11-20",
    messages=[
        {"role": "system", "content": "You are a helpful assistant."},
        {"role": "user", "content": "Hello!"}
    ],
    stream=stream
)

if stream:
    for chunk in completion:
        if len(chunk.choices) > 0 and chunk.choices[0].delta.content is not None:
            print(chunk.choices[0].delta.content, end="")
    print()
else:
    for chunk in completion.choices:
        print(chunk.message.content)

print('#### 视觉')
# 图片所支持的格式见: https://platform.openai.com/docs/guides/vision/what-type-of-files-can-i-upload

image_path = "./data/1.png"

def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')

completion = client.chat.completions.create(
    model="gpt-4o-2024-11-20",
    messages=[
        {
            "role": "user",
            "content": [
                {"type": "text", "text": "What's in this image?"},
                {
                    "type": "image_url",
                    "image_url": {
                        "url": f"data:image/jpeg;base64,{encode_image(image_path)}"
                    }
                }
            ],
        }
    ],
    stream=stream
)

if stream:
    for chunk in completion:
        if len(chunk.choices) > 0 and chunk.choices[0].delta.content is not None:
            print(chunk.choices[0].delta.content, end="")
    print()
else:
    print(completion)


print('#### 图片生成')
response = client.images.generate(
  model="dall-e-3",
  prompt="a white siamese cat",
  size="1024x1024",
  quality="standard",
  response_format='b64_json',
  style='vivid',
  n=1,
)
with open('./data/图片生成.png', 'wb') as f:
    f.write(base64.b64decode(response.data[0].b64_json))

print('#### 工具调用')
tools = [
    {
        "type": "function",
        "function": {
            "name": "get_current_weather",
            "description": "Get the current weather in a given location",
            "parameters": {
                "type": "object",
                "properties": {
                    "location": {
                        "type": "string",
                        "description": "The city and state, e.g. San Francisco, CA",
                    },
                    "unit": {"type": "string", "enum": ["celsius", "fahrenheit"]},
                },
                "required": ["location"],
            },
        }
    }
]
messages = [{"role": "user", "content": "What's the weather like in Boston today?"}]
completion = client.chat.completions.create(
    model="gpt-4o",
    messages=messages,
    tools=tools,
    tool_choice="auto",
    stream=stream
)

if stream:
    for chunk in completion:
        print(chunk)
else:
    print(completion)

print('#### 嵌入')
response = client.embeddings.create(
    model="text-embedding-ada-002",
    input="The food was delicious and the waiter...",
    encoding_format="float"
)
print(response)
