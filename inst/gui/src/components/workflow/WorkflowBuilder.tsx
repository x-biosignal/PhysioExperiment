import { useState } from 'react'

// Note: In production, this would use ReactFlow
// For now, we implement a basic version without the dependency

interface WorkflowNode {
  id: string
  type: 'input' | 'process' | 'output'
  label: string
  category: string
  x: number
  y: number
  config?: Record<string, unknown>
}

interface WorkflowEdge {
  id: string
  source: string
  target: string
}

const nodeTemplates = {
  input: [
    { type: 'input', label: 'データ読み込み', category: 'input' },
    { type: 'input', label: 'PhysioNet', category: 'input' },
    { type: 'input', label: 'OpenNeuro', category: 'input' },
  ],
  preprocess: [
    { type: 'process', label: 'フィルタ', category: 'preprocess' },
    { type: 'process', label: 'リファレンス変更', category: 'preprocess' },
    { type: 'process', label: 'リサンプリング', category: 'preprocess' },
    { type: 'process', label: 'ICA', category: 'preprocess' },
    { type: 'process', label: 'アーチファクト除去', category: 'preprocess' },
  ],
  analysis: [
    { type: 'process', label: 'スペクトログラム', category: 'analysis' },
    { type: 'process', label: 'ウェーブレット', category: 'analysis' },
    { type: 'process', label: 'バンドパワー', category: 'analysis' },
    { type: 'process', label: '接続性', category: 'analysis' },
    { type: 'process', label: 'エポッキング', category: 'analysis' },
  ],
  statistics: [
    { type: 'process', label: 't検定', category: 'statistics' },
    { type: 'process', label: 'ANOVA', category: 'statistics' },
    { type: 'process', label: '順列検定', category: 'statistics' },
  ],
  output: [
    { type: 'output', label: '保存', category: 'output' },
    { type: 'output', label: 'エクスポート', category: 'output' },
    { type: 'output', label: '可視化', category: 'output' },
  ],
}

interface NodePaletteProps {
  onDragStart: (template: { type: string; label: string; category: string }) => void
}

function NodePalette({ onDragStart }: NodePaletteProps) {
  const categories = [
    { id: 'input', label: '入力', color: 'bg-green-500' },
    { id: 'preprocess', label: '前処理', color: 'bg-blue-500' },
    { id: 'analysis', label: '解析', color: 'bg-purple-500' },
    { id: 'statistics', label: '統計', color: 'bg-orange-500' },
    { id: 'output', label: '出力', color: 'bg-red-500' },
  ]

  return (
    <div className="w-56 flex-shrink-0 border-r flex flex-col">
      <div className="border-b p-4">
        <h2 className="font-semibold">ノードパレット</h2>
        <p className="mt-1 text-xs text-muted-foreground">
          ドラッグしてキャンバスに追加
        </p>
      </div>
      <div className="flex-1 overflow-auto p-3 space-y-4">
        {categories.map((category) => (
          <div key={category.id}>
            <div className="flex items-center gap-2 mb-2">
              <div className={`h-2 w-2 rounded-full ${category.color}`} />
              <span className="text-sm font-medium">{category.label}</span>
            </div>
            <div className="space-y-1">
              {nodeTemplates[category.id as keyof typeof nodeTemplates]?.map((template, idx) => (
                <div
                  key={idx}
                  draggable
                  onDragStart={() => onDragStart(template)}
                  className="rounded border bg-card px-3 py-2 text-sm cursor-move hover:bg-accent transition-colors"
                >
                  {template.label}
                </div>
              ))}
            </div>
          </div>
        ))}
      </div>
    </div>
  )
}

interface WorkflowCanvasProps {
  nodes: WorkflowNode[]
  edges: WorkflowEdge[]
  selectedNode: string | null
  onNodeSelect: (id: string | null) => void
  onNodeMove: (id: string, x: number, y: number) => void
  onNodeDelete: (id: string) => void
  onDrop: (x: number, y: number) => void
}

function WorkflowCanvas({
  nodes,
  edges,
  selectedNode,
  onNodeSelect,
  onNodeMove,
  onNodeDelete,
  onDrop
}: WorkflowCanvasProps) {
  const [draggedNode, setDraggedNode] = useState<string | null>(null)
  const [dragOffset, setDragOffset] = useState({ x: 0, y: 0 })

  const getCategoryColor = (category: string) => {
    switch (category) {
      case 'input': return 'border-green-500 bg-green-500/10'
      case 'preprocess': return 'border-blue-500 bg-blue-500/10'
      case 'analysis': return 'border-purple-500 bg-purple-500/10'
      case 'statistics': return 'border-orange-500 bg-orange-500/10'
      case 'output': return 'border-red-500 bg-red-500/10'
      default: return 'border-gray-500'
    }
  }

  const handleMouseDown = (e: React.MouseEvent, nodeId: string) => {
    const node = nodes.find(n => n.id === nodeId)
    if (!node) return
    setDraggedNode(nodeId)
    setDragOffset({
      x: e.clientX - node.x,
      y: e.clientY - node.y
    })
    onNodeSelect(nodeId)
  }

  const handleMouseMove = (e: React.MouseEvent) => {
    if (!draggedNode) return
    onNodeMove(draggedNode, e.clientX - dragOffset.x, e.clientY - dragOffset.y)
  }

  const handleMouseUp = () => {
    setDraggedNode(null)
  }

  const handleDrop = (e: React.DragEvent) => {
    e.preventDefault()
    const rect = e.currentTarget.getBoundingClientRect()
    onDrop(e.clientX - rect.left, e.clientY - rect.top)
  }

  const handleDragOver = (e: React.DragEvent) => {
    e.preventDefault()
  }

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Delete' && selectedNode) {
      onNodeDelete(selectedNode)
    }
  }

  return (
    <div
      className="flex-1 relative bg-[url('data:image/svg+xml,%3Csvg%20width%3D%2220%22%20height%3D%2220%22%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%3E%3Ccircle%20cx%3D%221%22%20cy%3D%221%22%20r%3D%221%22%20fill%3D%22%23888%22%20opacity%3D%220.3%22%2F%3E%3C%2Fsvg%3E')]"
      onMouseMove={handleMouseMove}
      onMouseUp={handleMouseUp}
      onMouseLeave={handleMouseUp}
      onDrop={handleDrop}
      onDragOver={handleDragOver}
      onClick={() => onNodeSelect(null)}
      onKeyDown={handleKeyDown}
      tabIndex={0}
    >
      {/* SVG for edges */}
      <svg className="absolute inset-0 pointer-events-none">
        {edges.map((edge) => {
          const sourceNode = nodes.find(n => n.id === edge.source)
          const targetNode = nodes.find(n => n.id === edge.target)
          if (!sourceNode || !targetNode) return null

          const x1 = sourceNode.x + 80
          const y1 = sourceNode.y + 24
          const x2 = targetNode.x
          const y2 = targetNode.y + 24

          const midX = (x1 + x2) / 2

          return (
            <path
              key={edge.id}
              d={`M ${x1} ${y1} C ${midX} ${y1}, ${midX} ${y2}, ${x2} ${y2}`}
              stroke="hsl(var(--primary))"
              strokeWidth="2"
              fill="none"
              opacity="0.6"
            />
          )
        })}
      </svg>

      {/* Nodes */}
      {nodes.map((node) => (
        <div
          key={node.id}
          className={`absolute rounded-lg border-2 px-4 py-2 cursor-move transition-shadow ${
            getCategoryColor(node.category)
          } ${selectedNode === node.id ? 'ring-2 ring-primary shadow-lg' : ''}`}
          style={{ left: node.x, top: node.y }}
          onMouseDown={(e) => {
            e.stopPropagation()
            handleMouseDown(e, node.id)
          }}
        >
          <div className="font-medium text-sm whitespace-nowrap">{node.label}</div>
          {/* Connection points */}
          <div className="absolute left-0 top-1/2 -translate-x-1/2 -translate-y-1/2 w-3 h-3 rounded-full bg-primary border-2 border-background" />
          <div className="absolute right-0 top-1/2 translate-x-1/2 -translate-y-1/2 w-3 h-3 rounded-full bg-primary border-2 border-background" />
        </div>
      ))}

      {/* Empty state */}
      {nodes.length === 0 && (
        <div className="absolute inset-0 flex items-center justify-center text-muted-foreground">
          <div className="text-center">
            <svg className="mx-auto h-12 w-12" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M4 5a1 1 0 011-1h14a1 1 0 011 1v2a1 1 0 01-1 1H5a1 1 0 01-1-1V5zM4 13a1 1 0 011-1h6a1 1 0 011 1v6a1 1 0 01-1 1H5a1 1 0 01-1-1v-6zM16 13a1 1 0 011-1h2a1 1 0 011 1v6a1 1 0 01-1 1h-2a1 1 0 01-1-1v-6z" />
            </svg>
            <p className="mt-4">ノードをドラッグして追加</p>
            <p className="mt-1 text-sm">解析パイプラインを構築</p>
          </div>
        </div>
      )}
    </div>
  )
}

interface NodeConfigPanelProps {
  node: WorkflowNode | null
}

function NodeConfigPanel({ node }: NodeConfigPanelProps) {
  if (!node) {
    return (
      <div className="w-64 flex-shrink-0 border-l p-4">
        <div className="text-center text-muted-foreground">
          <p>ノードを選択して設定</p>
        </div>
      </div>
    )
  }

  return (
    <div className="w-64 flex-shrink-0 border-l flex flex-col">
      <div className="border-b p-4">
        <h2 className="font-semibold">{node.label}</h2>
        <p className="text-xs text-muted-foreground mt-1">ID: {node.id}</p>
      </div>
      <div className="flex-1 overflow-auto p-4">
        {/* Node-specific configuration would go here */}
        {node.category === 'preprocess' && node.label === 'フィルタ' && (
          <div className="space-y-4">
            <div>
              <label className="block text-sm font-medium">低域カットオフ (Hz)</label>
              <input
                type="number"
                defaultValue={0.1}
                className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
              />
            </div>
            <div>
              <label className="block text-sm font-medium">高域カットオフ (Hz)</label>
              <input
                type="number"
                defaultValue={40}
                className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm"
              />
            </div>
          </div>
        )}
        {node.category === 'analysis' && node.label === 'スペクトログラム' && (
          <div className="space-y-4">
            <div>
              <label className="block text-sm font-medium">ウィンドウサイズ</label>
              <select className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm">
                <option>128</option>
                <option>256</option>
                <option>512</option>
                <option>1024</option>
              </select>
            </div>
            <div>
              <label className="block text-sm font-medium">オーバーラップ</label>
              <select className="mt-1 w-full rounded-md border bg-background px-3 py-2 text-sm">
                <option>25%</option>
                <option>50%</option>
                <option>75%</option>
              </select>
            </div>
          </div>
        )}
        {/* Default message for other nodes */}
        {!['フィルタ', 'スペクトログラム'].includes(node.label) && (
          <p className="text-sm text-muted-foreground">
            このノードの設定オプションは開発中です。
          </p>
        )}
      </div>
    </div>
  )
}

export function WorkflowBuilder() {
  const [nodes, setNodes] = useState<WorkflowNode[]>([])
  const [edges, setEdges] = useState<WorkflowEdge[]>([])
  const [selectedNode, setSelectedNode] = useState<string | null>(null)
  const [dragTemplate, setDragTemplate] = useState<{ type: string; label: string; category: string } | null>(null)

  const handleDrop = (x: number, y: number) => {
    if (!dragTemplate) return

    const newNode: WorkflowNode = {
      id: `node-${Date.now()}`,
      type: dragTemplate.type as 'input' | 'process' | 'output',
      label: dragTemplate.label,
      category: dragTemplate.category,
      x,
      y,
      config: {}
    }

    setNodes([...nodes, newNode])
    setDragTemplate(null)
  }

  const handleNodeMove = (id: string, x: number, y: number) => {
    setNodes(nodes.map(node =>
      node.id === id ? { ...node, x, y } : node
    ))
  }

  const handleNodeDelete = (id: string) => {
    setNodes(nodes.filter(node => node.id !== id))
    setEdges(edges.filter(edge => edge.source !== id && edge.target !== id))
    if (selectedNode === id) {
      setSelectedNode(null)
    }
  }

  const handleRunWorkflow = () => {
    // TODO: Execute workflow via API
    console.log('Running workflow:', { nodes, edges })
  }

  const handleSaveWorkflow = () => {
    // TODO: Save workflow to file
    const workflow = JSON.stringify({ nodes, edges }, null, 2)
    console.log('Saving workflow:', workflow)
  }

  const handleClearWorkflow = () => {
    setNodes([])
    setEdges([])
    setSelectedNode(null)
  }

  const selectedNodeData = nodes.find(n => n.id === selectedNode) || null

  return (
    <div className="flex h-full flex-col">
      {/* Toolbar */}
      <div className="border-b px-4 py-2 flex items-center gap-4">
        <h1 className="font-semibold">ワークフロービルダー</h1>
        <div className="flex-1" />
        <button
          onClick={handleClearWorkflow}
          className="rounded px-3 py-1.5 text-sm hover:bg-accent"
        >
          クリア
        </button>
        <button
          onClick={handleSaveWorkflow}
          className="rounded border px-3 py-1.5 text-sm hover:bg-accent"
        >
          保存
        </button>
        <button
          onClick={handleRunWorkflow}
          disabled={nodes.length === 0}
          className="rounded bg-primary px-3 py-1.5 text-sm text-primary-foreground hover:bg-primary/90 disabled:opacity-50"
        >
          実行
        </button>
      </div>

      {/* Main content */}
      <div className="flex flex-1 overflow-hidden">
        <NodePalette onDragStart={setDragTemplate} />
        <WorkflowCanvas
          nodes={nodes}
          edges={edges}
          selectedNode={selectedNode}
          onNodeSelect={setSelectedNode}
          onNodeMove={handleNodeMove}
          onNodeDelete={handleNodeDelete}
          onDrop={handleDrop}
        />
        <NodeConfigPanel node={selectedNodeData} />
      </div>
    </div>
  )
}
