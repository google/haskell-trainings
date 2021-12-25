# Copyright 2021 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

.PHONY: run clean

TARGET = codelab
ARGS = solve

TOOL = .tool
BUILD_GENERATED = dist dist-newstyle .stack-work stack.yaml.lock $(TOOL)

run: $(TOOL)
	@`cat $(TOOL)` run $(TARGET) $(ARGS)

clean:
	@$(RM) -r $(BUILD_GENERATED)

#
# Private rules to check which toolchain is available
#

$(TOOL):
	@(test -e `which stack` && echo stack > $(TOOL) || true)
	@(test -s $(TOOL) || (test -e `which cabal` && echo cabal > $(TOOL)) || true)
	@(test -s $(TOOL) || (cat setup.md && false))
