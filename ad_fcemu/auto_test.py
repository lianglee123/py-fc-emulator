# -*- coding: utf-8 -*-
import datetime
import os
import time

from watchdog.events import PatternMatchingEventHandler
from watchdog.observers import Observer


class AutoTest(PatternMatchingEventHandler):
    patterns = ["*.py"]

    def process(self, event):
        print('auto test', os.getcwd())
        print(datetime.datetime.now().strftime("%Y/%m/%d %I:%M %p"))
        os.system('flake8 --max-line-length=120 && nosetests')

    def on_modified(self, event):
        self.process(event)


def main():
    path = '.'
    observer = Observer()
    observer.schedule(AutoTest(), path=path, recursive=True)
    observer.start()

    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        observer.stop()

    observer.join()


if __name__ == '__main__':
    main()
