
# 1. HTMLParser
<!-- TOC -->autoauto- [1. HTMLParser](#1-htmlparser)auto    - [1.1. Image Recognition](#11-image-recognition)auto    - [1.2. React Live with parsing in JS](#12-react-live-with-parsing-in-js)auto    - [1.3. React live with Haskell Parser](#13-react-live-with-haskell-parser)autoauto<!-- /TOC -->
## 1.1. Image Recognition
The image recognition part is in the **project_c** folder.
<br>
Create a virtual environment:
```bash
python3 -m venv ./venv
```
Activate virtual environment:
```bash
source ./venv/bin/activate opencv-python
```
Install dependencies:
```bash
python -m pip install flask flask-cors
```
Run app:
```bash
python beta_1_0_0.py
```
## 1.2. React Live with parsing in JS
Found in **project-live** folder.
Depends on **npm** and **node**.
<br>
Install dependencies:
```bash
cd project-live
npm install
```
Start application:
```bash
npm start
```
## 1.3. React live with Haskell Parser
Found in **project-output-haskell** folder.
Depends on **npm** and **node**.
Install dependencies:
```bash
cd project-live
npm install
```
```bash
cabal --new-install HTTP
```
Run react part:
```bash
npm start
```
Run haskell part:
```bash
project-output-haskell/haskell/fetch.sh "http://127.0.0.1:8080"
```
