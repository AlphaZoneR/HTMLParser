# Image Recognition
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
# React Live with parsing in JS
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
# React live with Haskell Parser
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
