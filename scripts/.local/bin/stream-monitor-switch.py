#!/usr/bin/env python3
import ipaddress
import os
import re
import subprocess
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

CONFIG = os.path.expanduser("~/.config/sunshine/sunshine.conf")
SERVICE = "app-dev.lizardbyte.app.Sunshine.service"

ALLOWED = [
    ipaddress.ip_network("127.0.0.1/32"),
    ipaddress.ip_network("100.64.0.0/10"),
    ipaddress.ip_network("192.168.0.0/16"),
    ipaddress.ip_network("172.16.0.0/12"),
    ipaddress.ip_network("10.0.0.0/8"),
    ipaddress.ip_network("::1/128"),
    ipaddress.ip_network("fc00::/7"),
]


def set_output_name(value):
    try:
        with open(CONFIG, "r") as f:
            lines = f.readlines()
    except FileNotFoundError:
        lines = []
    kept = [ln for ln in lines if not re.match(r"^\s*output_name\s*=", ln)]
    if value:
        kept.append("output_name = %s\n" % value)
    with open(CONFIG, "w") as f:
        f.writelines(kept)
    subprocess.run(["systemctl", "--user", "restart", SERVICE], check=True)


class Handler(BaseHTTPRequestHandler):
    def _respond(self, code, body):
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(body.encode())

    def do_GET(self):
        try:
            src = ipaddress.ip_address(self.client_address[0])
            if not any(src in net for net in ALLOWED):
                self._respond(403, '{"status":"denied"}')
                return
            if self.path == "/dp1":
                set_output_name("DP-1")
                self._respond(200, '{"status":"ok","output_name":"DP-1"}')
            elif self.path in ("/primary", "/"):
                set_output_name("")
                self._respond(200, '{"status":"ok","output_name":"<primary>"}')
            else:
                self._respond(404, '{"status":"not found"}')
        except Exception:
            self._respond(500, '{"status":"error"}')


if __name__ == "__main__":
    server = ThreadingHTTPServer(("0.0.0.0", 8123), Handler)
    server.serve_forever()