create an `.env` file, and 
```
OPENAI_API_KEY="sk-ACCESS_PASSWORD"
OPENAI_BASE_URL="https://chat.noc.pku.edu.cn/v1"
```

usage of llm:
```bash
$ python openai_test.py > ./data/1.out
```

test of funsearch
```bash
$ nohup python funsearch/run.py > nohup.out 2>&1 &
```

## reference
osfstorage-archive (data): https://osf.io/42b68/
