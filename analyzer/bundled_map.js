"use client";

// ../target-project/app/page.tsx
import { useSearchParams } from "next/navigation";
import { useEffect, useRef } from "react";
function VulnerabilityTest() {
  const searchParams = useSearchParams();
  const containerRef = useRef(null);
  useEffect(() => {
    const userPayload = searchParams.get("payload");
    if (userPayload && containerRef.current) {
      console.log("Injecting payload...");
      containerRef.current.innerHTML = userPayload;
    }
  }, [searchParams]);
  return /* @__PURE__ */ React.createElement("div", { style: { padding: 20 } }, /* @__PURE__ */ React.createElement("h1", null, "XSS Test Arena"), /* @__PURE__ */ React.createElement("p", null, "Whatever you put in the 'payload' URL parameter will be injected below:"), /* @__PURE__ */ React.createElement(
    "div",
    {
      ref: containerRef,
      style: { border: "2px dashed red", padding: 10, minHeight: 50 }
    }
  ));
}
export {
  VulnerabilityTest as default
};
//# sourceMappingURL=bundled_map.js.map
