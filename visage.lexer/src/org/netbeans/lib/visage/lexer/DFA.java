/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */
package org.netbeans.lib.visage.lexer;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;


/**
 * This class optimizes the lexer memory usage by detecting and removing
 * redundancy in the ANTLR state transition tables. It does so in a hacky way
 * by namespace-override of the ANTLR's own DFA to hijack
 * the unpackEncodedString call.
 *
 * Alternative solutions to this problem (2x9.5MB wasted) are:<ul>
 * <li>Patch ANTLR to generate more effective tables (patch was actually submitted)</li>
 * <li>Post-fix the Lexer class everytime after generation from the grammar</li>
 * </ul>
 *
 * @author nenik
 */
public class DFA extends org.antlr.runtime.DFA {
    private static Map<ArrayWrapper,ArrayWrapper> cache = new HashMap<ArrayWrapper, ArrayWrapper>();

    public static short[] getShared(short[] shorts) {
        ArrayWrapper wrapper = new ArrayWrapper(shorts);
        if (cache.containsKey(wrapper)) {
            wrapper = cache.get(wrapper);
        } else {
            cache.put(wrapper, wrapper);
        }
        return wrapper.array;
    }

    public static short[] unpackEncodedString(String def) {
        short[] shorts = org.antlr.runtime.DFA.unpackEncodedString(def);
        return getShared(shorts);
    }

    private static class ArrayWrapper {
        short[] array;
        int hashcode;

        ArrayWrapper(short[] data) {
            array = data;
            int sum = data.length;
            for (short v : data) {
                sum = sum * 33 + v;
            }
            hashcode = sum;
        }

        public @Override int hashCode() {
            return hashcode;
        }

        public @Override boolean equals(Object obj) {
            if (obj instanceof ArrayWrapper) {
                ArrayWrapper other = (ArrayWrapper)obj;
                if (other.hashcode != hashcode) return false;
                return Arrays.equals(other.array, array);
            }
            return false;
        }

    }
}
